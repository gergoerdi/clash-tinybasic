module TinyBASICSerial(
           input        CLK_32MHZ,
           input        RX,
           output       TX
           );

   topEntity u_topEntity
     (.CLK(CLK_32MHZ),
      .RESET(1'b0),
      .RX(RX),
      .TX(TX)
      );

endmodule
