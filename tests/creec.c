#define BYTE_WIDTH 8
#define BYTES_PER_BEAT 8
#define BEAT_WIDTH (BYTES_PER_BEAT * BYTE_WIDTH)
#include <stdio.h>

#include "creec_configs.h"
#include "mmio.h"

void header_write(uint32_t BASE_ADDR,
                  uint32_t len,
                  uint32_t compressed, uint32_t encrypted, uint32_t ecc,
                  uint32_t compressed_pad_bytes,
                  uint32_t encrypted_pad_bytes,
                  uint32_t ecc_pad_bytes) {
  reg_write32(BASE_ADDR + NUM_BEATS_IN_OFFSET, len);
  reg_write32(BASE_ADDR + CR_IN_OFFSET, compressed);
  reg_write32(BASE_ADDR + E_IN_OFFSET, encrypted);
  reg_write32(BASE_ADDR + ECC_IN_OFFSET, ecc);
  reg_write32(BASE_ADDR + CR_PADBYTES_IN_OFFSET, compressed_pad_bytes);
  reg_write32(BASE_ADDR + E_PADBYTES_IN_OFFSET, encrypted_pad_bytes);
  reg_write32(BASE_ADDR + ECC_PADBYTES_IN_OFFSET, ecc_pad_bytes);
}

void header_read(uint32_t BASE_ADDR, uint32_t *header) {
  header[0] = reg_read32(BASE_ADDR + NUM_BEATS_OUT_OFFSET);
  header[1] = reg_read32(BASE_ADDR + CR_OUT_OFFSET);
  header[2] = reg_read32(BASE_ADDR + E_OUT_OFFSET);
  header[3] = reg_read32(BASE_ADDR + ECC_OUT_OFFSET);
  header[4] = reg_read32(BASE_ADDR + CR_PADBYTES_OUT_OFFSET);
  header[5] = reg_read32(BASE_ADDR + E_PADBYTES_OUT_OFFSET);
  header[6] = reg_read32(BASE_ADDR + ECC_PADBYTES_OUT_OFFSET);
}

uint64_t pack_data(uint8_t *input, int i) {
  int j;
  uint64_t result = 0;
  for (j = i * BYTES_PER_BEAT; j < (i + 1) * BYTES_PER_BEAT; j++) {
    result = (result >> BYTES_PER_BEAT) |
		         ((uint64_t)input[j] << (BEAT_WIDTH - BYTE_WIDTH));
  }
  return result;
}

int main(void)
{
  uint8_t data[] = {1, 2, 3, 4, 5, 6, 7, 8,
                    2, 2, 2, 2, 2, 2, 2, 2,
                    2, 2, 2, 2, 2, 2, 2, 2,
                    0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 3,
                    3, 4, 4, 4, 4, 4, 4, 4};

  // This is the golden result obtained from applying Compression, Encryption,
  // and ECC on data
  int8_t gold_data[] = {31, 30, -88, -97, -3, -25, -53, 11,
                       -27, 97, 110, 35, 93, 81, 101, 76,
                       53, 90, 59, 62, -125, 20, -11, 67,
                       114, 79, 17, 32, 78, 26, 18, 1,
                       46, 55, 96, -75, -52, 33, 59, -41,
                       48, -20, -127, -36, 107, 69, -102, -8,
                       -30, -85, 3, 112, -15, -21, 6, 25,
                       10, -107, 88, 26, 109, -71, -85, -99};

  uint32_t ref_len = (uint32_t)(sizeof(gold_data) / BYTES_PER_BEAT);
  uint32_t ref_compressed = 1;
  uint32_t ref_encrypted = 1;
  uint32_t ref_ecc = 1;
  uint32_t ref_compressed_pad_bytes = 4;
  uint32_t ref_encrypted_pad_bytes = 8;
  uint32_t ref_ecc_pad_bytes = 0;

  int i, j;
  uint32_t len = sizeof(data) / BYTES_PER_BEAT;

  header_write(CREECW_ENABLE, len, 0, 0, 0, 0, 0, 0);
  reg_write32(CREECW_ENABLE, 1);

  for (i = 0; i < len; i++) {
    uint64_t packed_data = pack_data(data, i);
    printf("Sending data %lu\n", packed_data);
    reg_write64(WRITEQ_W, packed_data);
  }

  uint16_t counter = 0;
  while (reg_read32(CREECW_ENABLE + NUM_BEATS_OUT_OFFSET) == 0) {
    counter += 1;
//    if (i % 10 == 0) {
//        printf("counter %d\n", counter);
//    }
  }
  printf("finished at counter %d\n", counter);

  // Receive the CREECBus transaction header from creecW
  uint32_t headerW[7];
  header_read(CREECW_ENABLE, headerW);
  uint32_t lenW                  = headerW[0];
  uint32_t compressedW           = headerW[1];
  uint32_t encryptedW            = headerW[2];
  uint32_t eccW                  = headerW[3];
  uint32_t compressed_pad_bytesW = headerW[4];
  uint32_t encrypted_pad_bytesW  = headerW[5];
  uint32_t ecc_pad_bytesW        = headerW[6];

  printf("Received header: %d %d %d %d %d %d %d\n",
    lenW, compressedW, encryptedW, eccW,
    compressed_pad_bytesW, encrypted_pad_bytesW, ecc_pad_bytesW);

  printf("Expected header: %d %d %d %d %d %d %d\n",
      ref_len, ref_compressed, ref_encrypted, ref_ecc,
      ref_compressed_pad_bytes, ref_encrypted_pad_bytes, ref_ecc_pad_bytes);

  int numErrorsW = 0;

  int8_t data_outW[10 * BYTES_PER_BEAT];

  for (i = 0; i < lenW; i++) { 
    uint64_t output = reg_read64(READQ_W);
    for (j = 0; j < BYTES_PER_BEAT; j++) {
      int8_t out_data = output & 0xFF;
      data_outW[i * BYTES_PER_BEAT + j] = out_data;
      output = output >> BYTES_PER_BEAT;
      printf("OUT: (i=%d, j=%d) creecW=%d, gold=%d\n",
             i, j, out_data, gold_data[i * BYTES_PER_BEAT +j]);
      if (out_data != gold_data[i * BYTES_PER_BEAT + j]) {
        numErrorsW += 1;
        printf("Mismatch!\n");
      }
    }
  }

  if (numErrorsW == 0) {
    printf("creecW PASSED!\n");
  } else {
    printf("creecW FAILED with %d mismatches!\n", numErrorsW);
  }

    // Make some noise to test ECC capability
  data_outW[16 * 0 + 0] = 0;
  data_outW[16 * 0 + 1] = 1;
  data_outW[16 * 0 + 2] = 2;
  data_outW[16 * 0 + 3] = 3;

  data_outW[16 * 1 + 0] = 0;
  data_outW[16 * 1 + 1] = 1;
  data_outW[16 * 1 + 2] = 2;
  data_outW[16 * 1 + 3] = 3;

  data_outW[16 * 2 + 0] = 0;
  data_outW[16 * 2 + 1] = 1;
  data_outW[16 * 2 + 2] = 2;
  data_outW[16 * 2 + 3] = 3;

  data_outW[16 * 3 + 0] = 0;
  data_outW[16 * 3 + 1] = 1;
  data_outW[16 * 3 + 2] = 2;
  data_outW[16 * 3 + 3] = 3;

  data_outW[16 * 4 + 0] = 0;
  data_outW[16 * 4 + 1] = 1;
  data_outW[16 * 4 + 2] = 2;
  data_outW[16 * 4 + 3] = 3;

  data_outW[16 * 5 + 0] = 0;
  data_outW[16 * 5 + 1] = 1;
  data_outW[16 * 5 + 2] = 2;
  data_outW[16 * 5 + 3] = 3;

  data_outW[16 * 6 + 0] = 0;
  data_outW[16 * 6 + 1] = 1;
  data_outW[16 * 6 + 2] = 2;
  data_outW[16 * 6 + 3] = 3;

  data_outW[16 * 7 + 0] = 0;
  data_outW[16 * 7 + 1] = 1;
  data_outW[16 * 7 + 2] = 2;
  data_outW[16 * 7 + 3] = 3;

  header_write(CREECR_ENABLE, lenW,
               compressedW, encryptedW, eccW,
               compressed_pad_bytesW,
               encrypted_pad_bytesW,
               ecc_pad_bytesW);
  reg_write32(CREECR_ENABLE, 1);

  for (i = 0; i < lenW; i++) {
    uint64_t packed_data = pack_data(data_outW, i);
    printf("Sending data %lu\n", packed_data);
    reg_write64(WRITEQ_R, packed_data);
  }

  counter = 0;
  while (reg_read32(CREECR_ENABLE + NUM_BEATS_OUT_OFFSET) == 0) {
    counter += 1;
//    if (i % 10 == 0) {
//        printf("counter %d\n", counter);
//    }
  }
  printf("finished at counter %d\n", counter);

  // Receive the CREECBus transaction header from creecR
  uint32_t headerR[7];
  header_read(CREECR_ENABLE, headerR);
  uint32_t lenR                  = headerR[0];
  uint32_t compressedR           = headerR[1];
  uint32_t encryptedR            = headerR[2];
  uint32_t eccR                  = headerR[3];
  uint32_t compressed_pad_bytesR = headerR[4];
  uint32_t encrypted_pad_bytesR  = headerR[5];
  uint32_t ecc_pad_bytesR        = headerR[6];

  printf("Received header: %d %d %d %d %d %d %d\n",
    lenR, compressedR, encryptedR, eccR,
    compressed_pad_bytesR, encrypted_pad_bytesR, ecc_pad_bytesR);

  printf("Expected header: 6 0 0 0 0 0 0\n");

  int numErrorsR = 0;

  int8_t data_outR[10 * BYTES_PER_BEAT];

  for (i = 0; i < lenR; i++) { 
    uint64_t output = reg_read64(READQ_R);

    for (j = 0; j < BYTES_PER_BEAT; j++) {
      int8_t out_data = output & 0xFF;
      data_outR[i * BYTES_PER_BEAT + j] = out_data;
      output = output >> BYTES_PER_BEAT;
      printf("OUT: (i=%d, j=%d) creecR=%d, gold=%d\n",
             i, j, out_data, data[i * BYTES_PER_BEAT +j]);
      if (out_data != data[i * BYTES_PER_BEAT + j]) {
        numErrorsR += 1;
        printf("Mismatch!\n");
      }
    }
  }

  if (numErrorsR == 0) {
    printf("creecR PASSED!\n");
  } else {
    printf("creecR FAILED with %d mismatches!\n", numErrorsR);
  }

  printf("Done!\n");
	return 0;
}
