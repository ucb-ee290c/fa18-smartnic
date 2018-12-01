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

  int i, j;
  int len = sizeof(data) / DATA_WIDTH;

  //reg_write32(CREECW_LEN, len);
  for (i = 0; i < len; i++) {
    uint64_t packed_data = 0;
    for (j = i * DATA_WIDTH; j < (i + 1) * DATA_WIDTH; j++) {
      packed_data = (packed_data >> DATA_WIDTH) | ((uint64_t)data[j] << (BEAT_WIDTH - DATA_WIDTH));
    }
    printf("Sending data %lu\n", packed_data);
    reg_write64(CREECW_WRITE, packed_data);
  }

  int out_len = 8;
  for (i = 0; i < out_len; i++) { 
    uint64_t output = reg_read64(CREECW_READ);
    for (j = 0; j < DATA_WIDTH; j++) {
      int8_t out_data = output & 0xFF;
      output = output >> DATA_WIDTH;
      printf("Got out_data (i=%d, j=%d) %d\n", i, j, out_data);
    }
  }

  printf("Done!\n");
	return 0;
}
