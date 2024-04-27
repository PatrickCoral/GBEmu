use std::fs::read;

#[allow(non_camel_case_types)]
use crate::cpu::{Conditions, Registers_16, Registers_8};

#[allow(non_camel_case_types)]
enum Ops {
    nop,
    ld_r16_imm16(Registers_16, u16),
    ld_r16_a(Registers_16),
    ld_imm16_sp(u16),
    inc_r16(Registers_16),
    dec_r16(Registers_16),
    ad_hl_r16(Registers_16),
    inc_r8(Registers_8),
    dec_r8(Registers_8),
    ld_r8_imm8(u8),
    rlca,
    rrca,
    rla,
    rra,
    daa,
    cpl,
    scf,
    ccf,
    jr_imm8(u8),
    jr_cond_imm8(Conditions, u8),
    stop,
    ld_r8_r8(Registers_8, Registers_8),
    halt,
    add_a_r8(Registers_8),
    adc_a_r8(Registers_8),
    sub_a_r8(Registers_8),
    sbc_a_r8(Registers_8),
    and_a_r8(Registers_8),
    xor_a_r8(Registers_8),
    or_a_r8(Registers_8),
    cp_a_r8(Registers_8),
    add_a_imm8(u8),
    adc_a_imm8(u8),
    sub_a_imm8(u8),
    sbc_a_imm8(u8),
    and_a_imm8(u8),
    xor_a_imm8(u8),
    or_a_imm8(u8),
    cp_a_imm8(u8),
    ret_cond(Conditions),
    ret,
    reti,
    jp_cond_imm16(Conditions, u16),
    jp_imm16(u16),
    jp_hl,
    call_cond_imm16(Conditions, u16),
    call_imm16(u16),
    rst_tgt3(),
    pop_r16stk(Registers_16),
    push_r16stk(Registers_16),
    ldh_c_a(Conditions),
    ldh_imm8_a(u8),
    ld_imm16_a(u16),
    ldh_a_c(Conditions),
    ldh_a_imm8(u8),
    ld_a_imm16(u16),
    add_sp_imm8(u8),
    ld_hl_sp_imm8(u8),
    ld_sp_hl,
    di,
    ei,
    prefix,
    rlc_r8(Registers_8),
    rrc_r8(Registers_8),
    rl_r8(Registers_8),
    rr_r8(Registers_8),
    sla_r8(Registers_8),
    sra_r8(Registers_8),
    swap_r8(Registers_8),
    srl_r8(Registers_8),
    bit_b3_r8(u8, Registers_8),
    res_b3_r8(u8, Registers_8),
    set_b3_r8(u8, Registers_8),
}

fn load_ROM() {
	let rom = read("res/cpu_instrs.gb").unwrap();
	println!("{} {}", rom.len(), 0xFFFF);
}

#[cfg(test)]
mod tests{
	use super::*;

	#[test]
	fn load() {
		load_ROM();
	}
}