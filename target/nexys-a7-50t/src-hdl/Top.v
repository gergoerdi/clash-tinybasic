module Top(
           input        CLK100MHZ,
           input        UART_TXD_IN,
           output       UART_RXD_OUT,
           );

   topEntity u_topEntity
     (.CLK(CLK100MHZ),
      .RX(UART_TXD_IN),
      .TX(UART_RXD_OUT)
      );

endmodule
