use std::fs::read;

#[allow(non_camel_case_types)]
use crate::cpu::{Flags, Registers_16, Registers_8};

#[allow(non_camel_case_types)]
enum Ops {
    nop,
    ld_r16_imm16(Registers_16, u16),
    ld_r16mem_a(Registers_16),
    ld_a_r16mem(Registers_16),
    ld_imm16_sp(u16),
    inc_r16(Registers_16),
    dec_r16(Registers_16),
    add_hl_r16(Registers_16),
    inc_r8(Registers_8),
    dec_r8(Registers_8),
    ld_r8_imm8(Registers_8, u8),
    rlca,
    rrca,
    rla,
    rra,
    daa,
    cpl,
    scf,
    ccf,
    jr_imm8(u8),
    jr_cond_imm8(Flags, u8),
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
    ret_cond(Flags),
    ret,
    reti,
    jp_cond_imm16(Flags, u16),
    jp_imm16(u16),
    jp_hl,
    call_cond_imm16(Flags, u16),
    call_imm16(u16),
    rst_tgt3(),
    pop_r16stk(Registers_16),
    push_r16stk(Registers_16),
    ldh_c_a(Flags),
    ldh_imm8_a(u8),
    ld_imm16_a(u16),
    ldh_a_c(Flags),
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

fn get_instruction(opcode: (u8, u8, u8, u8, u8, u8, u8, u8)) -> Ops {
    match opcode {
        (0, 0, b5, b4, b3, b2, b1, b0) => match_block_0((b5, b4, b3, b2, b1, b0)),
        (0, 1, b5, b4, b3, b2, b1, b0) => match_block_1((b5, b4, b3, b2, b1, b0)),
        (1, 0, b5, b4, b3, b2, b1, b0) => match_block_2((b5, b4, b3, b2, b1, b0)),
        (1, 1, b5, b4, b3, b2, b1, b0) => match_block_3((b5, b4, b3, b2, b1, b0)),
        _ => todo!(),
    }
}

fn match_block_0(opcode: (u8, u8, u8, u8, u8, u8)) -> Ops {
    match opcode {
        (0, 0, 0, 0, 0, 0) => Ops::nop,
        (0, 0, 1, 0, 0, 0) => Ops::ld_imm16_sp(()),
        (0, 0, 0, 1, 1, 1) => Ops::rlca,
        (0, 0, 1, 1, 1, 1) => Ops::rrca,
        (0, 1, 0, 1, 1, 1) => Ops::rla,
        (0, 1, 1, 1, 1, 1) => Ops::rra,
        (1, 0, 0, 1, 1, 1) => Ops::daa,
        (1, 0, 1, 1, 1, 1) => Ops::cpl,
        (1, 1, 0, 1, 1, 1) => Ops::scf,
        (1, 1, 1, 1, 1, 1) => Ops::ccf,
        (0, 1, 1, 0, 0, 0) => Ops::jr_imm8(()),
        (0, 1, 0, 0, 0, 0) => Ops::stop,
		
        (r1, r0, 0, 0, 0, 1) => Ops::ld_r16_imm16(bits_to_r16(r1, r0), ()),
        (r1, r0, 0, 0, 1, 0) => Ops::ld_r16mem_a(bits_to_r16mem(r1, r0)),
        (r1, r0, 1, 0, 1, 0) => Ops::ld_a_r16mem(bits_to_r16mem(r1, r0)),
        (r1, r0, 0, 0, 1, 1) => Ops::inc_r16(bits_to_r16(r1, r0)),
        (r1, r0, 1, 0, 1, 1) => Ops::dec_r16(bits_to_r16(r1, r0)),
        (r1, r0, 1, 0, 0, 1) => Ops::add_hl_r16(bits_to_r16(r1, r0)),
        (r2, r1, r0, 1, 0, 0) => Ops::inc_r8(bits_to_r8(r2, r1, r0)),
        (r2, r1, r0, 1, 0, 1) => Ops::dec_r8(bits_to_r8(r2, r1, r0)),
        (r2, r1, r0, 1, 1, 0) => Ops::ld_r8_imm8(bits_to_r8(r2, r1, r0), ()),
        (1, c1, c0, 0, 0, 0) => Ops::jr_cond_imm8(bits_to_flags(c1, c0), ()),
    }
}

fn match_block_1(opcode: (u8, u8, u8, u8, u8, u8)) -> Ops {
    match opcode {
        (1, 1, 0, 1, 1, 0) => Ops::halt,
        (d2, d1, d0, s2, s1, s0) => Ops::ld_r8_r8(bits_to_r8(d2, d1, d0), bits_to_r8(s2, s1, s0)),
    }
}

fn match_block_2(opcode: (u8, u8, u8, u8, u8, u8)) -> Ops {
    match opcode {
        (0, 0, 0, r2, r1, r0) => Ops::add_a_r8(bits_to_r8(r2, r1, r0)),
        (0, 0, 1, r2, r1, r0) => Ops::adc_a_r8(bits_to_r8(r2, r1, r0)),
        (0, 1, 0, r2, r1, r0) => Ops::sub_a_r8(bits_to_r8(r2, r1, r0)),
        (0, 1, 1, r2, r1, r0) => Ops::sbc_a_r8(bits_to_r8(r2, r1, r0)),
        (1, 0, 0, r2, r1, r0) => Ops::and_a_r8(bits_to_r8(r2, r1, r0)),
        (1, 0, 1, r2, r1, r0) => Ops::xor_a_r8(bits_to_r8(r2, r1, r0)),
        (1, 1, 0, r2, r1, r0) => Ops::or_a_r8(bits_to_r8(r2, r1, r0)),
        (1, 1, 1, r2, r1, r0) => Ops::cp_a_r8(bits_to_r8(r2, r1, r0)),
    }
}

fn match_block_3(opcode: (u8, u8, u8, u8, u8, u8)) -> Ops {
    match opcode {
        (0, 0, 0, 1, 1, 0) => Ops::add_a_imm8(()),
        (0, 0, 1, 1, 1, 0) => Ops::adc_a_imm8(()),
        (0, 1, 0, 1, 1, 0) => Ops::sub_a_imm8(()),
        (0, 1, 1, 1, 1, 0) => Ops::sbc_a_imm8(()),
        (1, 0, 0, 1, 1, 0) => Ops::and_a_imm8(()),
        (1, 0, 1, 1, 1, 0) => Ops::xor_a_imm8(()),
        (1, 1, 0, 1, 1, 0) => Ops::or_a_imm8(()),
        (1, 1, 1, 1, 1, 0) => Ops::cp_a_imm8(()),
        (0, 0, 1, 0, 0, 1) => Ops::ret,
        (0, 1, 1, 0, 0, 1) => Ops::reti,
        (0, 0, 0, 0, 1, 1) => Ops::jp_imm16(()),
        (1, 0, 1, 0, 0, 1) => Ops::jp_hl,
        (0, 0, 1, 1, 0, 1) => Ops::call_imm16(()),
        (0, 0, 1, 0, 1, 1) => Ops::prefix,
        (1, 0, 0, 0, 1, 0) => Ops::ldh_c_a(()),
        (1, 0, 0, 0, 0, 0) => Ops::ldh_imm8_a(()),
        (1, 0, 1, 0, 1, 0) => Ops::ld_imm16_a(()),
        (1, 1, 0, 0, 1, 0) => Ops::ldh_a_c(()),
        (1, 1, 0, 0, 0, 0) => Ops::ldh_a_imm8(()),
        (1, 1, 1, 0, 1, 0) => Ops::ld_a_imm16(()),
        (1, 0, 1, 0, 0, 0) => Ops::add_sp_imm8(()),
        (1, 1, 1, 0, 0, 0) => Ops::ld_hl_sp_imm8(()),
        (1, 1, 1, 0, 0, 1) => Ops::ld_sp_hl,
        (1, 1, 0, 0, 1, 1) => Ops::di,
        (1, 1, 1, 0, 1, 1) => Ops::ei,

        (0, c1, c0, 0, 0, 0) => Ops::ret_cond(bits_to_flags(c1, c0)),
        (0, c1, c0, 0, 1, 0) => Ops::jp_cond_imm16(bits_to_flags(c1, c0), ()),
        (0, c1, c0, 1, 0, 0) => Ops::call_cond_imm16(bits_to_flags(c1, c0), ()),
        (t2, t1, t0, 1, 1, 1) => Ops::rst_tgt3(),
        (r1, r0, 0, 0, 0, 1) => Ops::pop_r16stk(bits_to_r16stk(r1, r0)),
        (r1, r0, 0, 1, 0, 1) => Ops::push_r16stk(bits_to_r16stk(r1, r0)),
    }
}

fn byte_to_tuple(opcode: u8) -> (u8, u8, u8, u8, u8, u8, u8, u8) {
    (
        opcode >> 7 & 1,
        opcode >> 6 & 1,
        opcode >> 5 & 1,
        opcode >> 4 & 1,
        opcode >> 3 & 1,
        opcode >> 2 & 1,
        opcode >> 1 & 1,
        opcode >> 0 & 1,
    )
}

fn bits_to_r16(r1: u8, r0: u8) -> Registers_16 {
    let r = r1 << 1 | r0;
    match r {
        0b00 => Registers_16::BC,
        0b01 => Registers_16::DE,
        0b10 => Registers_16::HL,
        0b11 => Registers_16::SP,
        _ => todo!(),
    }
}

fn bits_to_r16stk(r1: u8, r0: u8) -> Registers_16 {
    let r = r1 << 1 | r0;
    match r {
        0b00 => Registers_16::BC,
        0b01 => Registers_16::DE,
        0b10 => Registers_16::HL,
        0b11 => Registers_16::AF,
        _ => todo!(),
    }
}

fn bits_to_r16mem(r1: u8, r0: u8) -> Registers_16 {
    let r = r1 << 1 | r0;
    match r {
        0b00 => Registers_16::BC,
        0b01 => Registers_16::DE,
        0b10 => Registers_16::HLI,
        0b11 => Registers_16::HLD,
        _ => todo!(),
    }
}

fn bits_to_r8(r2: u8, r1: u8, r0: u8) -> Registers_8 {
    let r = r2 << 2 | r1 << 1 | r0;
    match r {
        0b000 => Registers_8::B,
        0b001 => Registers_8::C,
        0b010 => Registers_8::D,
        0b011 => Registers_8::E,
        0b100 => Registers_8::H,
        0b101 => Registers_8::L,
        0b110 => Registers_8::HL,
        0b111 => Registers_8::A,
    }
}

fn bits_to_flags(c1: u8, c0: u8) -> Flags {
    let c = c1 << 1 | c0;
    match c {
        0b00 => Flags::NZ,
        0b01 => Flags::Z,
        0b10 => Flags::NC,
        0b11 => Flags::C,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn load() {
        load_ROM();
    }
}
