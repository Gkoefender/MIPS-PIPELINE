------------------------------------------------------------------------------
--  Alunos: Guilherme Sergei Schäfer e Gustavo Koefender
--  E-mails: guilherme.schafer@acad.pucrs.br e gustavo.koefender@acad.pucrs.br
--  Professor: César Marcon
--  Código Original: Fernando Moraes e Ney Calazans
--	MIPS_MCS (MIPS Multi-Ciclo Single Edge)
------------------------------------------------------------------------------

-- package com os tipos básicos auxiliares para descrever o processador
library ieee;
    use ieee.std_logic_1164.all;

package p_MIPS_MCS is  
    -- inst_type define as instruções decodificáveis pelo bloco de controle
    type inst_type is ( 
        ADDU, SUBU, AAND, OOR, XXOR, NNOR, SSLL, SLLV, SSRA, SRAV,
        SSRL, SRLV, ADDIU, ANDI, ORI, XORI, LUI, LBU, LW, SB, SW, SLT,
        SLTU, SLTI,	SLTIU, BEQ, BGEZ, BLEZ, BNE, J, JAL, JALR, JR, 
        NOP, invalid_instruction
    );
    
    --------------------------------------------------------------------------
    --type microinstruction is record
    --    wpc:   std_logic;       -- PC write enable
    --    wreg:  std_logic;       -- register bank write enable
    --   ce:    std_logic;       -- Chip enable and R_W controls
    --    rw:    std_logic;
    --    bw:    std_logic;       -- Byte-word control (mem write only)
    --    i:     inst_type;       -- operation specification
    --end record;
    ---------------------------------------------------------------------------

    -- creation of if_id barrier
    type if_id is record
        npc  : std_logic_vector(31 downto 0);
        ir   : std_logic_vector(31 downto 0); -- object code of the instruction
    end record;

    -- creation of if_ex barrier
    type id_ex is record
        i             : inst_type;
        npc           : std_logic_vector(31 downto 0); -- new pc value  
        rs_data       : std_logic_vector(31 downto 0); -- target register data
        rt_data       : std_logic_vector(31 downto 0); -- target register data
        ext_data      : std_logic_vector(31 downto 0); -- offset with extended signal 
        rt_address    : std_logic_vector(4 downto 0);  -- target register address     
        rd_address:   : std_logic_vector(4 downto 0);  -- destination register address
        ULAFonte      : std_logic;
        RegDst        : std_logic;
        EscReg        : std_logic;
        MemParaReg    : std_logic;
        DvC           : std_logic;
        LerMem        : std_logic;
        EscMem        : std_logic;
    end record;
    -- creation of ex_mem barrier
    type ex_mem is record
        PC_Branch    : std_logic_vector(31 downto 0);
        alu_result   : std_logic_vector(31 downto 0);
        reg_dest   : std_logic_vector(4 downto 0);
        rt_data      : std_logic_vector(31 downto 0);
        zero_flag   : std_logic;
        EscReg        : std_logic;
        MemParaReg    : std_logic;
        DvC           : std_logic;
        LerMem        : std_logic;
        EscMem        : std_logic;
    end record;

    -- creation of mem_wb barrier
    type mem_wb is record
        Data_Mem     : std_logic_vector(31 downto 0);
        alu_result   : std_logic_vector(31 downto 0);
        reg_dest       : std_logic_vector(4 downto 0);    --Registrador a ser escrito
        EscReg        : std_logic;
        MemParaReg    : std_logic;
    end record;

end p_MIPS_MCS;




--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Registrador de uso geral - sensível à borda de subida do relógio (ck), 
--		com reset assíncrono (rst) e habilitaçãoo de escrita (ce)
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library ieee;
    use ieee.std_logic_1164.all;

entity regnbits is
    generic( INIT_VALUE : STD_LOGIC_VECTOR(31 downto 0) := (others=>'0') );
    port(  
        ck, rst, ce : in std_logic;
        D : in  STD_LOGIC_VECTOR (31 downto 0);
        Q : out STD_LOGIC_VECTOR (31 downto 0)
    );
end regnbits;

architecture regnbits of regnbits is 
begin
    process(ck, rst)
    begin
        if rst = '1' then
            Q <= INIT_VALUE(31 downto 0);
        elsif ck'event and ck = '1' then
            if ce = '1' then
                Q <= D; 
            end if;
        end if;
    end process;
end regnbits;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Banco de Registradores  (R0..R31) - 31 registradores de 32 bits
-- 	Trata-se de uma memória com três portas de acesso, não confundir
--	com a memória principal do processador. 
--	São duas portas de leitura (sinais AdRP1+DataRP1 e AdRP2+DataRP2) e
--  uma porta de escrita (dedfinida pelo conjunto de sinais 
--  ck, rst, ce, AdWP e DataWP).
--	Os endereços de cada porta (AdRP1, AdRP2 e AdWP) são obviamente de
--	5 bits (pois 2^5=32), enquanto que os barramentos de dados de 
--	saída (DataRP1, DataRP2) e de entrada (DataWP) são de 32 bits.
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library ieee;
    use ieee.std_Logic_1164.all;
    use ieee.std_logic_unsigned.all;   
    use work.p_MIPS_MCS.all;

entity reg_bank is
        port(  
            ck, rst, ce        : in std_logic       --ce é o WReg
            rs_address, rt_address, Reg_W : in std_logic_vector(4 downto 0);
            Reg_Data : in std_logic_vector(31 downto 0)
            DataRP1, DataRP2   : out std_logic_vector(31 downto 0) 
        );
end reg_bank;

architecture reg_bank of reg_bank is
    type wirebank is array(0 to 31) of std_logic_vector(31 downto 0);
    signal reg : wirebank ;                            
    signal wen : std_logic_vector(31 downto 0) ;
begin            
    g1: for i in 0 to 31 generate        
        -- Remember register $0 is the constant 0, not a register.
        -- This is implemented by never enabling writes to register $0
        wen(i) <= '1' when i/=0 and Reg_W=i and ce='1' else '0';
         
        -- Remember register $29, the stack pointer, points to some place
        -- near the bottom of the data memory, not the usual place 
		-- assigned by the MIPS simulator!!
        g2: if i=29 generate -- SP ---  x10010000 + x800 -- top of stack
            r29: entity work.regnbits generic map(INIT_VALUE=>x"10010800")    
                 port map(ck=>ck, rst=>rst, ce=>wen(i), D=>Reg_Data, Q=>reg(i));
        end generate;  
                
        g3: if i/=29 generate 
            rx: entity work.regnbits 
				port map(ck=>ck, rst=>rst, ce=>wen(i), D=>Reg_Data, Q=>reg(i));                    
        end generate;
    end generate g1;   

    DataRP1 <= reg(CONV_INTEGER(rt_address)); 
    DataRP2 <= reg(CONV_INTEGER(rs_address)); 
end reg_bank; 

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- ALU - Uma unidade lógico-aritmética puramente combinacional, cuja 
--		saída depende dos valores nas suas entradas de dados op1 e op2, cada
--		uma de 32 bits e da instrução sendo executada pelo processador
--		que é informada via o sinal de controle op_alu.
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library ieee;
    use ieee.std_logic_1164.all;
    use ieee.std_logic_unsigned.all;
    use ieee.std_logic_arith.all;
    use work.p_MIPS_MCS.all;

entity alu is
    port( 
        op1, op2 : in std_logic_vector(31 downto 0);
        outalu :   out std_logic_vector(31 downto 0);   
        op_alu :    in inst_type ;  
        zero_flag : out std_logic
    );
end alu;

architecture alu of alu is 
    signal menorU, menorS : std_logic ;
begin
  
    menorU <=  '1' when op1 < op2 else '0';
    menorS <=  '1' when ieee.Std_Logic_signed."<"(op1,  op2) else '0' ; -- signed
    
    zero_flag <= '1' when ( (op1=op1  and op_alu=BEQ)  or (op1>=0  and op_alu=BGEZ) or
                    (op1<=0  and op_alu=BLEZ) or (op1/=op2 and op_alu=BNE) )  else
                     '0';

    outalu <=  
        op1 - op2                            when  op_alu=SUBU                     else
        op1 and op2                          when  op_alu=AAND  or op_alu=ANDI     else 
        op1 or  op2                          when  op_alu=OOR   or op_alu=ORI      else 
        op1 xor op2                          when  op_alu=XXOR  or op_alu=XORI     else 
        op1 nor op2                          when  op_alu=NNOR                     else 
        op2(15 downto 0) & x"0000"           when  op_alu=LUI                      else
        (0=>menorU, others=>'0')             when  op_alu=SLTU  or op_alu=SLTIU    else
        (0=>menorS, others=>'0')             when  op_alu=SLT   or op_alu=SLTI     else
        op1(31 downto 28) & op2(27 downto 0) when  op_alu=J     or op_alu=JAL      else 
        op1                                  when  op_alu=JR    or op_alu=JALR     else
        x"00000000000"                       when  op_alu=NOP                      else 
        to_StdLogicVector(to_bitvector(op1) sll  CONV_INTEGER(op2(10 downto 6)))   when
													op_alu=SSLL   else 
        to_StdLogicVector(to_bitvector(op2) sll  CONV_INTEGER(op1(5 downto 0)))    when
													op_alu=SLLV   else 
        to_StdLogicVector(to_bitvector(op1) sra  CONV_INTEGER(op2(10 downto 6)))   when  
													op_alu=SSRA   else 
        to_StdLogicVector(to_bitvector(op2) sra  CONV_INTEGER(op1(5 downto 0)))    when  
													op_alu=SRAV   else 
        to_StdLogicVector(to_bitvector(op1) srl  CONV_INTEGER(op2(10 downto 6)))   when  
													op_alu=SSRL   else 
        to_StdLogicVector(to_bitvector(op2) srl  CONV_INTEGER(op1(5 downto 0)))    when
													op_alu=SRLV   else 
        op1 + op2;    -- default for ADDU,ADDIU,LBU,LW,SW,SB,BEQ,BGEZ,BLEZ,BNE    
end alu;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Descrição Estrutural do Bloco de Dados 
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library ieee;
    use ieee.Std_Logic_1164.all;
    use ieee.Std_Logic_signed.all; -- needed for comparison instructions SLTx
    use ieee.Std_Logic_arith.all;  -- needed for comparison instructions SLTxU
    use work.p_MIPS_MCS.all;
   
entity datapath is
    port(  
        ck, rst     :     in std_logic;
        d_address   :     out std_logic_vector(31 downto 0);
        data        :     inout std_logic_vector(31 downto 0); 
        IR_IN       :     in std_logic_vector(31 downto 0);
        i_address          :     in std_logic_vector(31 downto 0)
    );
end datapath;

architecture datapath of  datapath is
    signal  pc, incpc : std_logic_vector(31 donwto 0);
    --==Estagio 2
    signal      i_S_IN: inst_type
    signal       IR, npc_S : std_logic_vector(31 donwto 0);
    signal       sign_extend, sign_extend_S_OUT : std_logic_vector(31 donwto 0);
    signal       rs_address, rt_address, rd_address : std_logic_vector(4 downto 0);
    signal       ULAFonte, RegDst, DvC, EscMem, LerMem, EscReg, MemParaReg : std_logic; --esc_reg começa em 0
    --==Estagio 3
    signal      i_T: inst_type
    signal  npc_T, op1, rt_data_T, sign_extend_T : std_logic_vector(31 donwto 0);
    signal ULAFonte_T, RegDst_T, DvC_T, EscMem_T, LerMem_T, EscReg_T, MemParaReg_T, zero_flag_T : std_logic;
    signal  branch, outalu, reg_dest_T  :  std_logic_vector(31 donwto 0);
    signal  rt_address_T, rd_address_T  : std_logic_vector(4 donwto 0);
    --==Estagio 4
    signal   EscMem_F, LerMem_F, EscReg_F, MemParaReg_F, FontePC, zero_flag_F, DvC_F : std_logic;
    signal  outalu_F, rt_data_F, Data_Mem, branch_F  :  std_logic_vector(31 donwto 0);
    signal   reg_dest_F :  std_logic_vector(4 donwto 0);
    --==estagio 5
    signal  MemParaReg_5,EscReg_5 : std_logic;
    signal  alu_result_5, Data_mem_5, reg_data  :  std_logic_vector(31 donwto 0); 
    signal  reg_dest_5   :  std_logic_vector(4 donwto 0);

    signal      cte_im  op2,  : std_logic_vector(31 downto 0) := (others=> '0');

begin

    Barriers : entity work.control_unit 
    port map(
        ck=>ck, rst=>rst,
        --====OUT B1
        B1.ir=>IR,
        B1.npc=> npc_S,                       
        B1.ir(25 downto 21)=>rs_address,
        B1.ir(20 downto 16)=>rt_address,  B1.ir(15 downto 0)=>sign_extend, 
        B1.ir(15 downto 11)=>rd_address,
        --=== IN B2
        npc_B2=>npc_S, i_B2=>i_S_IN,
        rs_data=>rs_data, rt_data_B2=>rt_data,
        sign_extend=>sign_extend_S_OUT, rt_address=>rt_address , rd_address=>rd_address,
        ULAFonte=>ULAFonte, RegDst=>RegDst, EscReg_B2=>EscReg, MemParaReg_B2=>MemParaReg,
        DvC_B2=>DvC, LerMem_B2=>LerMem,  EscMem_B2=>EscMem,
        --== OUT B2
        B2.npc=>npc_T, B2.rs_data=>op1, B2.rt_data=>rt_data_T, B2.i=>i_T, B2.rt_address=>rt_address_T, B2.rs_address=>rd_address_T
        B2.ext_data=>sign_extend_T, B2.ULAFonte=>ULAFonte_T, B2.RegDst=>RegDst_T, B2.DvC=>DvC_T,
        B2.EscMem=>EscMem_T, B2.LerMem=>LerMem_T, B2.EscReg=>EscReg_T, B2.MemParaReg=>MemParaReg_T,
        --== IN B3
        PC_Branch=>branch, alu_result=>outalu, rt_data_B3=>rt_data_T,
        reg_dest_B3=>reg_dest_T, zero_flag=>zero_flag_T, DvC_B3=>DvC_T, EscMem_B3=>EscMem_T, LerMem_B3=>LerMem_T,
        EscReg_B3=>EscReg_T, MemParaReg_B3=>MemParaReg_T,
        --== OUT B3
        B3.zero_flag=>zero_flag_F, B3.PC_Branch=>branch_F, B3.DvC=>DvC_F
        B3.alu_result=>alu_result_F, =>B3.rt_data=>rt_data_F,
        B3.reg_dest=>Reg_Dest_4,
        --== IN B4
        data_mem=>Data_Mem, alu_result_B4=>alu_result_F,
        Reg_Dest_B4=>reg_dest_F, MemParaReg_B4=>MemParaReg_F, EscReg_B4,EscReg_F ,
        --== OUT B4
        B4.reg_dest=>Reg_Dest_5, B4.EscReg=>EscReg_5, B4.MemParaReg=>MemParaReg_5, B4.alu_result=>alu_result_5,
        B4.Data_Mem=> Data_mem_5
    );

        --INCREMENTO DO PC NAO FINALIZADO
      REGS: entity work.reg_bank(reg_bank) port map
             (rs_address=>rs_address, DataRP1=>rs_data_S, rt_address=>rt_address, DataRP2=>rt_data_S,
                 ck=>ck, rst=>rst, ce=>EscReg_5, Reg_W=>reg_dest_5, Reg_Data=>reg_data);

       RPC: entity work.regnbits generic map(INIT_VALUE=>x"00400000")   
                            port map(ck=>ck, rst=>rst, ce=>, D=>dtpc, Q=>pc);
       NPC_OUT <= npc;
       incpc <= pc + 4;


    i_address <= pc;  -- connects PC output to the instruction memory address bus

   --==============================================================================
   -- second stage
   --==============================================================================


    RegDst     <= '0' when i_S_IN=LW else '1';
    ULAFonte   <= '1' when i_S_IN=LW or I_S_IN=SW else '0';
    DvC        <= '1' when i_S_IN=BEQ else '0';
    LerMem     <= '1' when i_S_IN=LW else '0';
    EscMem     <= '1' when i_S_IN=SW else '0';
    EscReg     <= '0' when i_S_IN=SW or i_S_IN=BEQ else '1';
    MemParaReg <= '1' when i_S_IN=LW else '0'; 

    
    i_S_IN <= ADDU    when IR(31 downto 26)="000000" and IR(10 downto 0)="00000100001" else
            NOP     when IR(31 downto 0)=x"00000000" else
            SUBU    when IR(31 downto 26)="000000" and IR(10 downto 0)="00000100011" else
            AAND    when IR(31 downto 26)="000000" and IR(10 downto 0)="00000100100" else
            OOR     when IR(31 downto 26)="000000" and IR(10 downto 0)="00000100101" else
            XXOR    when IR(31 downto 26)="000000" and IR(10 downto 0)="00000100110" else
            NNOR    when IR(31 downto 26)="000000" and IR(10 downto 0)="00000100111" else
            SSLL    when IR(31 downto 21)="00000000000" and IR(5 downto 0)="000000" else
            SLLV    when IR(31 downto 26)="000000" and IR(10 downto 0)="00000000100" else
            SSRA    when IR(31 downto 21)="00000000000" and IR(5 downto 0)="000011" else
            SRAV    when IR(31 downto 26)="000000" and IR(10 downto 0)="00000000111" else
            SSRL    when IR(31 downto 21)="00000000000" and IR(5 downto 0)="000010" else
            SRLV    when IR(31 downto 26)="000000" and IR(10 downto 0)="00000000110" else
            ADDIU   when IR(31 downto 26)="001001" else
            ANDI    when IR(31 downto 26)="001100" else
            ORI     when IR(31 downto 26)="001101" else
            XORI    when IR(31 downto 26)="001110" else
            LUI     when IR(31 downto 26)="001111" else
            LW      when IR(31 downto 26)="100011" else
            LBU     when IR(31 downto 26)="100100" else
            SW      when IR(31 downto 26)="101011" else
            SB      when IR(31 downto 26)="101000" else
            SLTU    when IR(31 downto 26)="000000" and IR(5 downto 0)="101011" else
            SLT     when IR(31 downto 26)="000000" and IR(5 downto 0)="101010" else
            SLTIU   when IR(31 downto 26)="001011"                             else
            SLTI    when IR(31 downto 26)="001010"                             else
            BEQ     when IR(31 downto 26)="000100" else
            BGEZ    when IR(31 downto 26)="000001" and IR(20 downto 16)="00001" else
            BLEZ    when IR(31 downto 26)="000110" and IR(20 downto 16)="00000" else
            BNE     when IR(31 downto 26)="000101" else
            J       when IR(31 downto 26)="000010" else
            JAL     when IR(31 downto 26)="000011" else
            JALR    when IR(31 downto 26)="000000"  and IR(20 downto 16)="00000"
                    and IR(10 downto 0) = "00000001001" else
            JR      when IR(31 downto 26)="000000" and IR(20 downto 0)="000000000000000001000" else 
    invalid_instruction ; -- IMPORTANT: default condition is invalid instruction;
    assert i /= invalid_instruction
    report "******************* INVALID INSTRUCTION *************"
    severity error;
           


    sign_extend <=  x"FFFF" & IR(15 downto 0) when IR(15)='1' else
             x"0000" & IR(15 downto 0);
    sign_extend_S_OUT <= 
            --sign_extend(29 downto 0)  & "00"     when inst_branch='1'			else
            --branch address adjustment for word frontier
            "0000" & IR_IN(25 downto 0) & "00" when i_S_IN=J or i_S_IN=JAL 		else
           --  J/JAL are word addressed. MSB four bits are defined at the ALU, not here!
            x"0000" & IR_IN(15 downto 0) when i_S_IN=ANDI or i_S_IN =ORI  or i_S_IN=XORI 	else
            -- logic instructions with immediate operand are zero extended
            sign_extend;
            -- The default case is used by addiu, lbu, lw, sbu and sw instructions

 
    --==============================================================================
    -- third stage
    --==============================================================================
    
    inst_R_sub  <= '1' when i_T=ADDU or i_T=SUBU or i_T=AAND
                    or i_T=OOR or i_T=XXOR 
                     or i_T=NNOR or i_T=NOP  else
                     '0';


    alu_operator2 <= '1' when inst_R_sub='1' or i_T=SLTU
                        or i_T=SLT or i_T=JR or i_T=SLLV or i_T=SRAV or i_T=SRLV 
                        else '0';

                            
    op2 <= rt_data_T when alu_operator2='0' else extend_data_T;                              --op2 mux decide

    branch <= (sign_extend_T_in(29 downto 0)  & "00") + npc_T;
     --sign_extend(29 downto 0)  & "00"     when inst_branch='1'

    reg_dest_T <= rt_address_T when RegDst_T='0' else rd_address_T;

    -- ALU instantiation
    DALU: entity work.alu port map (
            op1=>op1, op2=>op2, outalu=>outalu, op_alu=>i_T, zero_flag=>zero_flag_T                         
        );

    --==============================================================================
    -- fourth stage
    --==============================================================================
     
    d_address <= outalu_F;
    -- tristate to control memory write    
    data <= RT when (EscMem_F='1') else (others=>'Z');  

    FontePC = zero_flag_F and DvC_F;
    dtpc <= npc when FontePC='0' else branch_F;

    --==============================================================================
    -- fifth stage
    --==============================================================================

    reg_dest_5 <= alu_result_5 when MemParaReg_5='1' else Data_mem_5;
    
    
end datapath;

--------------------------------------------------------------------------
--  Descrição do Bloco de Controle (mista, estrutural-comportamental)
--------------------------------------------------------------------------
library ieee;
    use ieee.Std_Logic_1164.all;
    use ieee.Std_Logic_unsigned.all;
    use work.p_MIPS_MCS.all;

entity control_unit is
        port(	
            ck, rst : in std_logic;
            --==B1 
            npc, instruction  : in std_logic_vector(31 downto 0);
            --==B2 
            i_B2 : in inst_type;
            npc_B2 : in std_logic_vector(31 downto 0);
            rs_data, rt_data_B2, rt_address, rd_address, sign_extend : in std_logic_vector(31 downto 0);
            ULAFonte, RegDst, DvC_B2, LerMem_B2, EscMem_B2, EscReg_B2, MemParaReg_B2 : in std_logic;
            --==B3
            DvC_B3, EscReg_B3, MemParaReg_B3, LerMem_B3, EscMem_B3, zero_flag :in std_logic;
            PC_Branch, alu_result_B3, rt_data_B3, Reg_dest_B3  : in std_logic_vector(31 downto 0);
            --==B4
            data_mem, alu_result_B4, reg_dest_B4 : in std_logic_vector(31 downto 0);
            Esc_Reg_B4, MemParaReg_B4 : in std_logic;

            B1   : out if_id;
            B2   : out id_ex;
            B3   : out ex_mem;
            B4   : out mem_wb
        );
end control_unit;
                   
architecture control_unit of control_unit is
   

begin

    process(ck,rst)
        begin
             if rst = '1' then
                B1.npc <= '0';
                B1.ir  <= '0';
             elsif ck'event and ck = '0' then
                B1.npc <= npc;
                B1.ir  <= instruction;
            end if;
    end process;

    process(ck,rst)
    begin
         if rst = '1' then
            B2.i            <= '0';
            B2.npc          <= '0'; 
            B2.rs_data      <= '0';
            B2.rt_data      <= '0';   
            B2.ext_data     <= '0';
            B2.rt_address   <= '0';
            B2.rd_address   <= '0';
            B2.ULAFonte     <= '0';
            B2.RegDst       <= '0'; 
            B2.EscReg       <= '0';
            B2.DvC          <= '0';
            B2.LerMem       <= '0';
            B2.EscMem       <= '0';
         elsif ck'event and ck = '0' then
            B2.i            <= i_B2;
            B2.npc          <= npc_B2; 
            B2.rs_data      <= rs_data;
            B2.rt_data      <= rt_data_B2;    
            B2.ext_data     <= sign_extend;
            B2.rt_address   <= rt_address;
            B2.rd_address   <= rd_address;
            B2.ULAFonte     <= ULAFonte;
            B2.RegDst       <= RegDst;
            B2.EscReg       <= EscReg_B2;
            B2.MemParaReg   <= MemParaReg_B2;
            B2.DvC          <= DvC_B2;
            B2.LerMem       <= LerMem_B2;
            B2.EscMem       <= EscMem_B2;
         end if;
    end process;



    process(ck,rst)
    begin
         if rst = '1' then
            B3.PC_Branch  <= '0';
            B3.alu_result <= '0';
            B3.rt_data    <= '0';
            B3.reg_dest   <= '0';
            B3.zero_flag  <= '0';            
            B3.EscReg     <= '0';
            B3.MemParaReg <= '0';
            B3.DvC        <= '0';
            B3.LerMem     <= '0';
            B3.EscMem     <= '0';
         elsif ck'event and ck = '0' then
            B3.PC_Branch  <= PC_Branch;
            B3.alu_result <= alu_result_B3;
            B3.rt_data    <= rt_data_B3;
            B3.reg_dest   <= reg_dest_B3;
            B3.zero_flag  <= zero_flag;            
            B3.EscReg     <= EscReg_B3;
            B3.MemParaReg <= MemParaReg_B3;
            B3.DvC        <= DvC_B3;
            B3.LerMem     <= LerMem_B3;
            B3.EscMem     <= EscMem_B3;
        end if;
    end process;


    process(ck,rst)
    begin
         if rst = '1' then
            B4.Data_Mem     <= '0';
            B4.alu_result   <= '0';
            B4.reg_dest     <= '0';
            B4.EscReg       <= '0';
            B4.MemParaReg   <= '0';
         elsif ck'event and ck = '0' then
            B4.Data_Mem     <= data_mem;
            B4.alu_result   <= alu_result_B4;
            B4.reg_dest     <= reg_dest_B4;
            B4.EscReg       <= Esc_Reg_B4;
            B4.MemParaReg   <= MemParaReg_B4;
        end if;
    end process;

    
end control_unit;

--------------------------------------------------------------------------
-- Processador MIPS_S completo, onde se instanciam BD e BC
--------------------------------------------------------------------------
library ieee;
    use ieee.std_logic_1164.all;
    use work.p_MIPS_MCS.all;

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
      signal IR, NPC : std_logic_vector(31 downto 0);
 begin

    dp: entity work.datapath   
        port map(
            ck=>clock, rst=>reset, d_address=>d_address, data=>data,
            uins=>uins, IR_IN=>IR, NPC_IN=>NPC, i_address=>i_address,
            instruction=>instruction,
        );

 --   ct: entity work.control_unit port map( 
 --      IR_OUT=>IR, NPC_OUT=>NPC
    );

     
end MIPS_MCS;