//100M MII Ethernet TX module
module tx_mac (

    input        clk_tx,

    //User interface signals
    input        tx_vld,
    input        tx_eof,
    input  [3:0] tx_dat,
    output       tx_ack,

    //MII phy signals
    output       mii_tx_en,
    reg    [3:0] mii_tx_dat

);

//Packet formatter state machine
reg [5:0] packet_cnt;

always @(posedge clk_tx) 
    if (packet_cnt == 0)       //Idle
        packet_cnt <= packet_cnt + tx_vld;
    else if (packet_cnt < 16)  //Preamble
        packet_cnt <= packet_cnt + 1;
    else if (packet_cnt == 16) //Data
        packet_cnt <= packet_cnt + tx_eof;
    else if (packet_cnt < 48) //FCS + interpacket gap
        packet_cnt <= packet_cnt + 1;
    else
        packet_cnt <= 0;

//Ack if we're in a state were we can accept data
assign tx_ack    = packet_cnt == 16;

//Set the MII output signals
assign mii_tx_en = (tx_vld || |packet_cnt) && (packet_cnt <= 24);

always @(*) 
    //preamble
    if (packet_cnt <= 14)
        mii_tx_dat = 4'h5;
    //SFD
    else if (packet_cnt == 15)
        mii_tx_dat = 4'hd;
    //data
    else if (packet_cnt == 16)
        mii_tx_dat = tx_dat;
    //CRC
    else
        mii_tx_dat = {!crc_final[28], !crc_final[29], !crc_final[30], !crc_final[31]};


//CRC calculation
integer i;
reg [31:0] crc_i [0:4];

reg [31:0] crc_final = 32'hffffffff;

always @(posedge clk_tx) 
    if (tx_vld && packet_cnt == 16)
        crc_final <= crc_i[4];
    else
        crc_final <= {crc_final[27:0], 4'hf};

always @(*) begin

    crc_i[0] = crc_final;

    for (i=0; i<4; i = i + 1)
        if (tx_dat[i] == crc_i[i][31]) 
           crc_i[i + 1] = {crc_i[i][30:0], 1'b0};
        else
           crc_i[i + 1] = {crc_i[i][30:0], 1'b0} ^ 32'h04C11DB7;
end

endmodule
