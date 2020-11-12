module TinyBASICVideo(
           input        CLK100MHZ,
           input        UART_TXD_IN,
           output       UART_RXD_OUT,
           input        PS2_CLK,
           input        PS2_DATA,
           output       VGA_HS,
           output       VGA_VS,
           output [3:0] VGA_R,
           output [3:0] VGA_G,
           output [3:0] VGA_B
           );

   wire                 CLK_25MHZ;
   wire                 CLK_LOCKED;
   wire [7:0]           VGA_RED_FULL;
   wire [7:0]           VGA_GREEN_FULL;
   wire [7:0]           VGA_BLUE_FULL;

   assign UART_RXD_OUT = 1'b0;
   
   assign VGA_R = VGA_RED_FULL[7:4];
   assign VGA_G = VGA_GREEN_FULL[7:4];
   assign VGA_B = VGA_BLUE_FULL[7:4];

   ClockWiz25 u_ClockWiz25
     (.CLKIN_100MHZ(CLK100MHZ),
      .CLKOUT_25MHZ(CLK_25MHZ),
      .LOCKED(CLK_LOCKED),
      .reset(1'b0)
      );

   topEntity u_topEntity
     (.CLK_25MHZ(CLK_25MHZ),
      .RESET(!CLK_LOCKED),
      .RX(UART_TXD_IN),
      .PS2_CLK(PS2_CLK),
      .PS2_DATA(PS2_DATA),
      .VGA_HSYNC(VGA_HS),
      .VGA_VSYNC(VGA_VS),
      .VGA_RED(VGA_RED_FULL),
      .VGA_GREEN(VGA_GREEN_FULL),
      .VGA_BLUE(VGA_BLUE_FULL)
      );

endmodule
