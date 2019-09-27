//100M MII Ethernet RX module
//Very minimal. Only strips the preamble. No error checking.
module rx_mac (

    input        clk_rx,

    //User interface signals
    output       rx_vld,
    output       rx_eof,
    output [3:0] rx_dat,

    //MII phy signals
    input        mii_rx_dv,
    input  [3:0] mii_rxd
);

//Strip the preamble
reg [4:0] packet_cnt = 0;

always @(posedge clk_rx)
    if (!mii_rx_dv)
        packet_cnt <= 0;
    else if (packet_cnt < 25)
        packet_cnt <= packet_cnt + 1;

//Strip the FCS
reg [35:0] data_shift = 0;

always @(posedge clk_rx)
    data_shift <= {data_shift[31:0], mii_rxd};

//Assign the outputs
assign rx_vld = packet_cnt == 25;
assign rx_dat = data_shift[35:32];
assign rx_eof = !mii_rx_dv;

endmodule
