------------------------------------------------------------------------------
--                         MIPS Subset with Pipeline                        --
--  Authors:                                                                --
--       Guilherme Sergei Schäfer                                           --
--       Gustavo Koefender                                                  --
--  E-mails:                                                                --
--       guilherme.schafer@acad.pucrs.br                                    --
--       gustavo.koefender@acad.pucrs.br                                    --
------------------------------------------------------------------------------

-- package with the auxiliaries types to describe the processor
-- in this package will be defined all the temporal barriers and the instruction type
library ieee;
    use ieee.std_logic_1164.all;

package p_MIPS_S is  
    -- instType defines all possible instructions that can be decoded in the control unit
    type instType is ( 
        ADDU, SUBU, AAND, OOR, XXOR, NNOR, SSLL, SLLV, SSRA, SRAV,
        SSRL, SRLV, ADDIU, ANDI, ORI, XORI, LUI, LBU, LW, SB, SW, SLT,
        SLTU, SLTI,	SLTIU, BEQ, BGEZ, BLEZ, BNE, J, JAL, JALR, JR, 
        NOP, invalid_instruction
    );

    -- creation of if_id barrier
    type if_id is record
        npc  : std_logic_vector(31 downto 0); -- keep the value of the new program counter
        ir   : std_logic_vector(31 downto 0); -- object code of the instruction
    end record;

    -- creation of if_ex barrier
    type id_ex is record
        ir             : std_logic_vector(31 downto 0); -- object code of the instruction
        npc            : std_logic_vector(31 downto 0); -- new pc value
        rsData         : std_logic_vector(31 downto 0); -- source register data
        rsAddress      : std_logic_vector(4  downto 0); -- source register address
        rtData         : std_logic_vector(31 downto 0); -- target register data
        rtAddress      : std_logic_vector(4  downto 0); -- target register address 
        rdAddress      : std_logic_vector(4  downto 0); -- destination register address
        extData        : std_logic_vector(31 downto 0); -- offset with extended signal
        inst_branch    : std_logic;                     -- signal to check if it is a branch instruction
        inst_R_sub     : std_logic;                     -- signal to check if it is a instruction type R sub
        instruction    : instType;                      -- which instruction
        aluOperator    : std_logic;                     -- control signal to determine which register will be used as operator
        extSignal : std_logic_vector(31 downto 0);
    end record;
    
    -- creation of ex_mem barrier
    type ex_mem is record
        ir               : std_logic_vector(31 downto 0); -- object code of the instruction
        npc              : std_logic_vector(31 downto 0); -- new pc value
        instruction      : instType;                      -- which instruction
        branchAddress    : std_logic_vector(31 downto 0); -- calculated branch address
        aluResult        : std_logic_vector(31 downto 0); -- calculated alu result
        inst_branch      : std_logic;                     -- signal to check if it is a branch instruction
        inst_R_sub       : std_logic;                     -- signal to check if it is a instruction type R sub
        rtData           : std_logic_vector(31 downto 0); -- calculated alu result
        willBranch       : std_logic;                     -- signal to control whether or not it will branch
        regDestAddress   : std_logic_vector(4  downto 0); -- address of the register in which the data will be written
    end record;

    -- creation of mem_wb barrier
    type mem_wb is record
        branchAddress    : std_logic_vector(31 downto 0);
        willBranch       : std_logic; 
        ir               : std_logic_vector(31 downto 0); -- object code of the instruction
        npc              : std_logic_vector(31 downto 0); -- new pc value
        instruction      : instType;                       -- which instruction
        dataRead         : std_logic_vector(31 downto 0); -- data that was read in memory
        inst_branch      : std_logic;                     -- signal to check if it is a branch instruction
        inst_R_sub       : std_logic;                     -- signal to check if it is a instruction type R sub
        aluResult        : std_logic_vector(31 downto 0); -- calculated alu result
        writeRegister    : std_logic;                     -- determines whether or not to write to the register
        regDestAddress   : std_logic_vector(4  downto 0); -- address of the register in which the data will be written
    end record;
end p_MIPS_S;


-- general purpose register
library ieee;
    use ieee.std_logic_1164.all;

entity genericRegister is
    -- the init_value can be defined or will be 0
    generic( INIT_VALUE : STD_LOGIC_VECTOR(31 downto 0) := (others=>'0') );
    port(  
        ck, rst, ce : in std_logic;
        D : in  STD_LOGIC_VECTOR (31 downto 0);
        Q : out STD_LOGIC_VECTOR (31 downto 0)
    );
end genericRegister;

architecture genericRegister of genericRegister is 
begin
    process(ck, rst)
    begin
        if rst = '1' then -- if reset is 0 then the register assumes the initial value
            Q <= INIT_VALUE(31 downto 0);
        elsif rising_edge(ck) then  -- rising edge clock sensitivity
            if ce = '1' then -- check chip enable
                Q <= D; -- copy data
            end if;
        end if;
    end process;
end genericRegister;

-- definition of the register bank
-- containing all 32 registers of mips
library ieee;
    use ieee.std_Logic_1164.all;
    use ieee.std_logic_unsigned.all;   
    use work.p_MIPS_S.all;

entity reg_bank is
    port(  
        ck, rst, ce : in std_logic;
        addressReadPort1, addressReadPort2, addressWritePort : in std_logic_vector( 4 downto 0);
        dataWritePort                   : in  std_logic_vector(31 downto 0);
        dataReadPort1, dataReadPort2    : out std_logic_vector(31 downto 0) 
    );
end reg_bank;

architecture reg_bank of reg_bank is
    type wirebank is array(0 to 31) of std_logic_vector(31 downto 0);
    signal reg : wirebank;                            
    signal writeEnable : std_logic_vector(31 downto 0);
begin            
    g1: for i in 0 to 31 generate        
        -- the write is enable when the address write port is the register number itself
        -- and the chip enable is 1. Otherwise it is disabled.
        -- if i = 0 then it's the register $zero, which always holds 0x0000 value
        -- in order to achieve that the write to it is disabled.
        writeEnable(i) <= '1' when i/=0 and addressWritePort=i and ce='1' else '0';
        
        -- generating the registers
        -- if it is the register number 29 ($sp) then assigns the MARS simulation value to it
        g2: if i=29 generate -- stackpointer: 0x10010000 + 0x800 = top of stack
            r29: entity work.genericRegister generic map(INIT_VALUE=>x"10010800")    
                 port map(ck=>ck, rst=>rst, ce=>writeEnable(i), D=>dataWritePort, Q=>reg(i));
        end generate;  
                
        g3: if i/=29 generate 
            rx: entity work.genericRegister 
				port map(ck=>ck, rst=>rst, ce=>writeEnable(i), D=>dataWritePort, Q=>reg(i));                    
        end generate;
    end generate g1;   

    dataReadPort1 <= reg(CONV_INTEGER(addressReadPort1)); -- source1 selection 
    dataReadPort2 <= reg(CONV_INTEGER(addressReadPort2)); -- source2 selection 
end reg_bank;

-- definition of the alu
library ieee;
    use ieee.std_logic_1164.all;
    use ieee.std_logic_unsigned.all;
    use ieee.std_logic_arith.all;
    use work.p_MIPS_S.all;

entity alu is
    port( 
        op1, op2 : in  std_logic_vector(31 downto 0);
        outalu   : out std_logic_vector(31 downto 0);   
        op_alu   : in  instType   
    );
end alu;

architecture alu of alu is 
    signal menorU, menorS : std_logic ;
begin
    menorU <=  '1' when op1 < op2 else '0';
    menorS <=  '1' when ieee.Std_Logic_signed."<"(op1,  op2) else '0' ; -- signed
    
    outalu <=  
        op1 - op2                             when  op_alu=SUBU                     else
        op1 and op2                           when  op_alu=AAND  or op_alu=ANDI     else 
        op1 or  op2                           when  op_alu=OOR   or op_alu=ORI      else 
        op1 xor op2                           when  op_alu=XXOR  or op_alu=XORI     else 
        op1 nor op2                           when  op_alu=NNOR                     else 
        op2(15 downto 0) & x"0000"            when  op_alu=LUI                      else
        (0=>menorU, others=>'0')              when  op_alu=SLTU  or op_alu=SLTIU    else
        (0=>menorS, others=>'0')              when  op_alu=SLT   or op_alu=SLTI     else
        op1(31 downto 28) & op2(27 downto 0)  when  op_alu=J     or op_alu=JAL      else 
        op1                                   when  op_alu=JR    or op_alu=JALR     else
        x"00000000"                           when  op_alu=NOP                      else 
        to_StdLogicVector(to_bitvector(op1)   sll   CONV_INTEGER(op2(10 downto 6))) when
												   op_alu=SSLL   else 
        to_StdLogicVector(to_bitvector(op2)   sll   CONV_INTEGER(op1(5 downto 0)))  when
												   op_alu=SLLV   else 
        to_StdLogicVector(to_bitvector(op1)   sra   CONV_INTEGER(op2(10 downto 6))) when  
												   op_alu=SSRA   else 
        to_StdLogicVector(to_bitvector(op2)   sra   CONV_INTEGER(op1(5 downto 0)))  when  
												   op_alu=SRAV   else 
        to_StdLogicVector(to_bitvector(op1)   srl   CONV_INTEGER(op2(10 downto 6))) when  
												   op_alu=SSRL   else 
        to_StdLogicVector(to_bitvector(op2)   srl   CONV_INTEGER(op1(5 downto 0)))  when
												   op_alu=SRLV   else 
        op1 + op2;    -- default for ADDU,ADDIU,LBU,LW,SW,SB,BEQ,BGEZ,BLEZ,BNE    
end alu;

-- datapath description
library ieee;
    use ieee.Std_Logic_1164.all;
    use ieee.Std_Logic_signed.all; -- needed for comparison instructions SLTx
    use ieee.Std_Logic_arith.all;  -- needed for comparison instructions SLTxU
    use work.p_MIPS_S.all;
   
entity datapath is
    port(  
        ce, rw, bw      :   out   std_logic;
        ck, rst         :   in    std_logic;
        regDestAddress  :   inout std_logic_vector(4 downto 0);   -- address of the register in which data will be written
        data            :   inout std_logic_vector(31 downto 0);   -- the data that will be read/write
        willBranch      :   inout std_logic;                       -- signal to control whether will or not branch
        branchAddress   :   inout std_logic_vector(31 downto 0);   -- calculated branch address
        regDestData     :   inout std_logic_vector(31 downto 0);   -- data that will be written in the destination register
        ir              :   in    std_logic_vector(31 downto 0);   -- object code of the instruction
        npc             : 	inout std_logic_vector(31 downto 0);   -- new pc value
        if_id           :   inout if_id;  -- first  temporal barrier
        id_ex           :   inout id_ex;  -- second temporal barrier
        ex_mem          :   inout ex_mem; -- third  temporal barrier
        mem_wb          :   inout mem_wb; -- fourth temporal barrier
        i_address       :   out   std_logic_vector(31 downto 0);
        d_address       :   out   std_logic_vector(31 downto 0)
    );
end datapath;

architecture datapath of  datapath is
    signal offset        : std_logic_vector(31 downto 0) := (others=> '0');
    signal rsAddress     : std_logic_vector(4 downto 0) := (others=> '0');
    signal rtAddress     : std_logic_vector(4 downto 0) := (others=> '0');
    signal rdAddress     : std_logic_vector(4 downto 0) := (others=> '0');
    signal rsData        : std_logic_vector(31 downto 0) := (others=> '0');
    signal rtData        : std_logic_vector(31 downto 0) := (others=> '0');
    signal extData       : std_logic_vector(31 downto 0) := (others=> '0');
    signal extSignal     : std_logic_vector(31 downto 0) := (others=> '0');
    signal aluOp1        : std_logic_vector(31 downto 0) := (others=> '0');
    signal aluOp2        : std_logic_vector(31 downto 0) := (others=> '0');
    signal aluResult     : std_logic_vector(31 downto 0) := (others=> '0');
    signal dataRead      : std_logic_vector(31 downto 0) := (others=> '0');
    signal aluOperator   : std_logic;
    signal writeRegister : std_logic;
    signal pc, incpc     : std_logic_vector(31 downto 0) := (others=> '0');
    signal instruction   : instType;
    signal inst_branch, inst_R_sub, inst_I_sub: std_logic; -- auxiliary signals
begin

    --==============================================================================
    -- first stage
    --==============================================================================

    rw    <= '0' when (ex_mem.instruction=SB or  ex_mem.instruction=SW) else  '1';
    ce    <= '1' when (ex_mem.instruction=LBU or ex_mem.instruction=LW or ex_mem.instruction=SB or ex_mem.instruction=SW)  else '0';
    bw    <= '0' when (ex_mem.instruction=LBU or ex_mem.instruction=SB) else '1';

    -- PC value
    i_address <= pc;
    pc <= ex_mem.branchAddress when ex_mem.willBranch = '1' or (ex_mem.instruction = J 
          or ex_mem.instruction = JAL or ex_mem.instruction = JALR or ex_mem.instruction = JR) else 
          incpc;
    npc <= pc + 4;
    incpc <= if_id.npc;

    firstBarrier: process(ck, rst)
	begin
		if rst = '1' then
            if_id.ir <=   x"00000000";
            if_id.npc <=  x"00400000";
		elsif rising_edge(ck) then 
			if_id.ir <= ir;
            if_id.npc <= npc;
		end if;
	end process firstBarrier;

    --==============================================================================
    -- second stage
    --==============================================================================
    
    secondBarrier: process(ck, rst)
	begin
        if rst = '1' then
            id_ex.rsAddress   <= "00000";
            id_ex.rtAddress   <= "00000";
            id_ex.rdAddress   <= "00000";
            id_ex.instruction <= NOP;
		elsif rising_edge(ck) then 
			id_ex.ir  <= if_id.ir;
            id_ex.npc <= if_id.npc;
            id_ex.rsData <= rsData;
            id_ex.rsAddress <= rsAddress;
            id_ex.rtData <= rtData;
            id_ex.rtAddress <= rtAddress;
            id_ex.rdAddress <= rdAddress;
            id_ex.extData <= extData;
            id_ex.instruction <= instruction;
            id_ex.aluOperator <= aluOperator;
            id_ex.inst_R_sub <= inst_R_sub;
            id_ex.inst_branch <= inst_branch;
            id_ex.extSignal <= extSignal;
		end if;
	end process secondBarrier;
    
    instruction <=
        ADDU    when if_id.ir(31 downto 26) = "000000" and if_id.ir(10 downto 0) = "00000100001" else
        NOP     when if_id.ir(31 downto 0)  = x"00000000"                                        else
        SUBU    when if_id.ir(31 downto 26) = "000000" and if_id.ir(10 downto 0) = "00000100011" else
        AAND    when if_id.ir(31 downto 26) = "000000" and if_id.ir(10 downto 0) = "00000100100" else
        OOR     when if_id.ir(31 downto 26) = "000000" and if_id.ir(10 downto 0) = "00000100101" else
        XXOR    when if_id.ir(31 downto 26) = "000000" and if_id.ir(10 downto 0) = "00000100110" else
        NNOR    when if_id.ir(31 downto 26) = "000000" and if_id.ir(10 downto 0) = "00000100111" else
        SSLL    when if_id.ir(31 downto 21) = "00000000000" and if_id.ir(5 downto 0) = "000000"  else
        SLLV    when if_id.ir(31 downto 26) = "000000" and ir(10 downto 0) = "00000000100"       else
        SSRA    when if_id.ir(31 downto 21) = "00000000000" and ir(5 downto 0) = "000011"        else
        SRAV    when if_id.ir(31 downto 26) = "000000" and if_id.ir(10 downto 0) = "00000000111" else
        SSRL    when if_id.ir(31 downto 21) = "00000000000" and if_id.ir(5 downto 0) = "000010"  else
        SRLV    when if_id.ir(31 downto 26) = "000000" and if_id.ir(10 downto 0) = "00000000110" else
        ADDIU   when if_id.ir(31 downto 26) = "001001" else
        ANDI    when if_id.ir(31 downto 26) = "001100" else
        ORI     when if_id.ir(31 downto 26) = "001101" else
        XORI    when if_id.ir(31 downto 26) = "001110" else
        LUI     when if_id.ir(31 downto 26) = "001111" else
        LW      when if_id.ir(31 downto 26) = "100011" else
        LBU     when if_id.ir(31 downto 26) = "100100" else
        SW      when if_id.ir(31 downto 26) = "101011" else
        SB      when if_id.ir(31 downto 26) = "101000" else
        SLTU    when if_id.ir(31 downto 26) = "000000" and if_id.ir(5 downto 0) = "101011"  else
        SLT     when if_id.ir(31 downto 26) = "000000" and if_id.ir(5 downto 0) = "101010"  else
        SLTIU   when if_id.ir(31 downto 26) = "001011"                                      else
        SLTI    when if_id.ir(31 downto 26) = "001010"                                      else
        BEQ     when if_id.ir(31 downto 26) = "000100"                                      else
        BGEZ    when if_id.ir(31 downto 26) = "000001" and if_id.ir(20 downto 16) = "00001" else
        BLEZ    when if_id.ir(31 downto 26) = "000110" and if_id.ir(20 downto 16) = "00000" else
        BNE     when if_id.ir(31 downto 26) = "000101" else
        J       when if_id.ir(31 downto 26) = "000010" else
        JAL     when if_id.ir(31 downto 26) = "000011" else
        JALR    when if_id.ir(31 downto 26) = "000000" and if_id.ir(20 downto 16) = "00000"
                                                       and if_id.ir(10 downto 0)  = "00000001001"           else
        JR      when if_id.ir(31 downto 26) = "000000" and if_id.ir(20 downto 0)  = "000000000000000001000" else
        invalid_instruction;

    -- determines if it is a branch instruction
    inst_branch <= '1' when instruction=BEQ or instruction=BGEZ or instruction=BLEZ or instruction=BNE else '0';

    -- inst_R_sub is a subset of R-type instructions
    inst_R_sub  <=  '1'  when instruction=ADDU or instruction=SUBU or instruction=AAND
                         or instruction=OOR or instruction=XXOR 
                         or instruction=NNOR or instruction=NOP  else '0';

    -- inst_I is a subset of I-type instructions
    inst_I_sub  <= '1' when instruction=ADDIU or instruction=ANDI or instruction=ORI or instruction=XORI else '0';
    
    rsAddress <= if_id.ir(20 downto 16) when instruction=SSLL or instruction=SSRA or instruction=SSRL else 
    if_id.ir(25 downto 21);

    rtAddress <= if_id.ir(20 downto 16);

    rdAddress <= if_id.ir(15 downto 11);
          
    -- instantiation of the registers
    REGS: entity work.reg_bank(reg_bank) port map (
        addressReadPort1=>rsAddress, dataReadPort1=>rsData, addressReadPort2=>rtAddress, dataReadPort2=>rtData,
        ck=>ck, rst=>rst, ce=>mem_wb.writeRegister, addressWritePort=>mem_wb.regDestAddress, dataWritePort=>regDestData
    );
    
    -- sign extension 
    extSignal <=    x"FFFF" & if_id.ir(15 downto 0) when if_id.ir(15)='1' else
                    x"0000" & if_id.ir(15 downto 0);
    
    -- Immediate constant
    extData <= extSignal(29 downto 0) & "00" when inst_branch='1' else
        "0000" & if_id.ir(25 downto 0) & "00" when instruction=J or instruction=JAL else
        x"0000" & if_id.ir(15 downto 0) when instruction=ANDI or instruction=ORI  or instruction=XORI else
        extSignal;
 
    regDestAddress <= id_ex.rdAddress when id_ex.inst_R_sub='1' else id_ex.rtAddress;

    branchAddress <= id_ex.npc + aluResult;

    -- evaluation of conditions to take the branch instructions
    willBranch <=  '1' when ((id_ex.rsData = id_ex.rtData and id_ex.instruction = BEQ)  or 
                            (id_ex.rsData >=0  and id_ex.instruction=BGEZ) or
                            (id_ex.rsData <= 0  and id_ex.instruction=BLEZ) or 
                            (id_ex.rsData /= id_ex.rtData and id_ex.instruction=BNE)) else 
                    '0';

    --==============================================================================
    -- third stage
    --==============================================================================
                      
    thirdBarrier: process(ck, rst)
	begin
        if rst = '1' then
            ex_mem.willBranch <= '0';
            ex_mem.regDestAddress   <= "00000";
            ex_mem.instruction <= NOP;
		elsif rising_edge(ck) then 
			ex_mem.ir  <= id_ex.ir;
            ex_mem.npc <= id_ex.npc;
            ex_mem.instruction <= id_ex.instruction;
            ex_mem.branchAddress <= aluresult;
            ex_mem.aluResult <= aluResult;
            ex_mem.rtData <= id_ex.rtData;
            ex_mem.inst_R_sub <= id_ex.inst_R_sub;
            ex_mem.inst_branch <= id_ex.inst_branch;
            ex_mem.willBranch <= willBranch;  
            ex_mem.regDestAddress <= regDestAddress;
		end if;
	end process thirdBarrier;

    -- select the first ALU operand                           
    aluOp1 <= id_ex.npc when id_ex.inst_branch='1' else id_ex.rsData; 
     
    -- select the second ALU operand
    aluOp2 <= id_ex.rtData when id_ex.inst_R_sub='1' or id_ex.instruction=SLTU or id_ex.instruction=SLT or id_ex.instruction=JR 
                  or id_ex.instruction=SLLV or id_ex.instruction=SRAV or id_ex.instruction=SRLV else id_ex.extData; 
                 
    -- ALU instantiation
    DALU: entity work.alu port map (
            op1=>aluOp1, op2=>aluOp2, outalu=>aluResult, op_alu=>id_ex.instruction
        );
     
    
    --==============================================================================
    -- fourth stage
    --==============================================================================
    
    fourthBarrier: process(ck, rst)
	begin
        if rst = '1' then
            mem_wb.regDestAddress <= "00000";
            mem_wb.writeRegister <= '0';
            mem_wb.instruction <= NOP;
		elsif rising_edge(ck) then 
			mem_wb.ir  <= ex_mem.ir;
            mem_wb.npc <= ex_mem.npc;
            mem_wb.instruction <= ex_mem.instruction;
            mem_wb.dataRead <= dataRead;
            mem_wb.aluResult <= ex_mem.aluResult;
            mem_wb.writeRegister <= writeRegister;
            mem_wb.regDestAddress <= ex_mem.regDestAddress;
            mem_wb.inst_R_sub <= ex_mem.inst_R_sub;
            mem_wb.inst_branch <= ex_mem.inst_branch;
		end if;
	end process fourthBarrier;

    writeRegister <= '0' when (ex_mem.inst_branch = '1' or ex_mem.instruction = J or 
                               ex_mem.instruction = JR or ex_mem.instruction = SW) else '1';

    d_address <= ex_mem.aluResult;
    
    data <= ex_mem.rtData when (ex_mem.instruction = SW) else (others=>'Z'); 

    -- single byte reading from memory  -- assuming the processor is little endian
    dataRead <= data when ex_mem.instruction = LW  else x"000000" & data(7 downto 0);

    --==============================================================================
    -- fifth stage
    --==============================================================================
    
    regDestData <= mem_wb.dataRead when mem_wb.instruction = LW else mem_wb.aluResult;
end datapath;


-- instatiation of the processor
library ieee;
    use ieee.std_logic_1164.all;
    use work.p_MIPS_S.all;

entity MIPS_MCS is
    port( 
        clock, reset: in std_logic;
        ce, rw, bw: out std_logic;
        i_address, d_address: out std_logic_vector(31 downto 0);
        instruction: in std_logic_vector(31 downto 0);
        data: inout std_logic_vector(31 downto 0)
    );
end MIPS_MCS;

architecture MIPS_MCS of MIPS_MCS is
    signal  NPC : std_logic_vector(31 downto 0);
 begin

    dp: entity work.datapath   
        port map(
            ck=>clock, rst=>reset, d_address=>d_address, data=>data,	   
            npc=>NPC, i_address=>i_address, ir=>instruction, ce=>ce, rw=>rw, bw=>bw
        );

end MIPS_MCS;