#define CREECW_WRITE 0x2000
#define CREECW_WRITE_COUNT 0x2008
#define CREECW_READ 0x2100
#define CREECW_READ_COUNT 0x2108
#define CREECW_LEN 0x2200

#define BEAT_WIDTH 64 // 64-bit per beat
#define DATA_WIDTH 8  // 8-bit per data of a beat

#include <stdio.h>

#include "mmio.h"

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

  int i, j;
  int len = sizeof(data) / DATA_WIDTH;

  // TODO
  //reg_write32(CREECW_LEN, len);

  for (i = 0; i < len; i++) {
    uint64_t packed_data = 0;
    for (j = i * DATA_WIDTH; j < (i + 1) * DATA_WIDTH; j++) {
      packed_data = (packed_data >> DATA_WIDTH) | ((uint64_t)data[j] << (BEAT_WIDTH - DATA_WIDTH));
    }
    printf("Sending data %lu\n", packed_data);
    reg_write64(CREECW_WRITE, packed_data);
  }

  // FIXME: Get output length from creecW
  int out_len = 8;
  int numErrors = 0;
  for (i = 0; i < out_len; i++) { 
    uint64_t output = reg_read64(CREECW_READ);
    for (j = 0; j < DATA_WIDTH; j++) {
      int8_t out_data = output & 0xFF;
      output = output >> DATA_WIDTH;
      printf("OUT: (i=%d, j=%d) creecW=%d, gold=%d\n",
             i, j, out_data, gold_data[i * DATA_WIDTH +j]);
      if (out_data != gold_data[i * DATA_WIDTH + j]) {
        numErrors += 1;
        printf("Mismatch!\n");
      }
    }
  }

  if (numErrors == 0) {
    printf("creecW PASSED!\n");
  } else {
    printf("creecW FAILED with %d mismatches!\n", numErrors);
  }

  printf("Done!\n");
	return 0;
}
