use std::any::Any;

use crate::{
    mmu::MMU,
    parser::{self, Ops},
};

#[allow(non_snake_case)]

pub(crate) enum Registers_16 {
    AF,
    BC,
    DE,
    HL,
    SP,
    HLI,
    HLD,
}

pub(crate) enum Registers_8 {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    HL,
}

pub(crate) enum Flags {
    NZ,
    Z,
    NC,
    C,
}

#[allow(non_snake_case)]
pub(crate) struct CPU {
    MMU: MMU,
    AF: u16,
    BC: u16,
    DE: u16,
    HL: u16,
    SP: u16,
    PC: u16,
}

impl CPU {
    pub(crate) fn init() -> Self {
        Self {
            MMU: MMU::init(),
            AF: 0,
            BC: 0,
            DE: 0,
            HL: 0,
            SP: 0,
            PC: 0x0150,
        }
    }

    pub(crate) fn set_flags(&mut self, flags: u8) {
        let flags = (flags & 0b1111_0000) as u16;
        let mask = !(0b1111_0000 as u16);
        self.AF &= mask;
        self.AF |= flags;
    }

    pub(crate) fn set_zero_flag(&mut self, flag: bool) {
        let flags = (flag as u16) << 7;
        let mask = !(0b1000_0000 as u16);
        self.AF &= mask;
        self.AF |= flags;
    }

    pub(crate) fn set_sub_flag(&mut self, flag: bool) {
        let flags = (flag as u16) << 6;
        let mask = !(0b0100_0000 as u16);
        self.AF &= mask;
        self.AF |= flags;
    }

    pub(crate) fn set_half_carry_flag(&mut self, flag: bool) {
        let flags = (flag as u16) << 5;
        let mask = !(0b0010_0000 as u16);
        self.AF &= mask;
        self.AF |= flags;
    }

    pub(crate) fn set_carry_flag(&mut self, flag: bool) {
        let flags = (flag as u16) << 4;
        let mask = !(0b0001_0000 as u16);
        self.AF &= mask;
        self.AF |= flags;
    }

    fn read_next_byte(&mut self) -> u8 {
        let byte = self.MMU.mmap[self.PC as usize];
        self.PC += 1;
        byte
    }

    pub(crate) fn next(&mut self) {
        let opcode: u8 = self.read_next_byte();
        match parser::get_instruction(opcode) {
            Ops::nop => return,
            Ops::ld_r16_imm16(d) => {
                let mut imm16 = (self.read_next_byte() as u16) << 8;
                imm16 |= self.read_next_byte() as u16;

                match d {
                    Registers_16::AF => self.AF = imm16,
                    Registers_16::BC => self.BC = imm16,
                    Registers_16::DE => self.DE = imm16,
                    Registers_16::HL => self.HL = imm16,
                    Registers_16::SP => self.SP = imm16,
                    Registers_16::HLI => {
                        self.HL = imm16;
                        self.HL += 1;
                    }
                    Registers_16::HLD => {
                        self.HL = imm16;
                        self.HL -= 1;
                    }
                }
            }
            Ops::ld_r16mem_a(d) => {
                let addr = match d {
                    Registers_16::AF => self.AF,
                    Registers_16::BC => self.BC,
                    Registers_16::DE => self.DE,
                    Registers_16::HL => self.HL,
                    Registers_16::SP => self.SP,
                    Registers_16::HLI => panic!(),
                    Registers_16::HLD => panic!(),
                };
                self.MMU.mmap[addr as usize] = (self.AF >> 8) as u8;
            }
            Ops::ld_a_r16mem(s) => {
                let addr = match s {
                    Registers_16::AF => self.AF,
                    Registers_16::BC => self.BC,
                    Registers_16::DE => self.DE,
                    Registers_16::HL => self.HL,
                    Registers_16::SP => self.SP,
                    Registers_16::HLI => panic!(),
                    Registers_16::HLD => panic!(),
                };

                self.AF &= 0x00FF;
                self.AF |= (self.MMU.mmap[addr as usize] as u16) & 0xFF00;
            }
            Ops::ld_imm16_sp => {
                let addr = (self.read_next_byte() as u16) << 8 | (self.read_next_byte() as u16);
                self.MMU.mmap[addr as usize] = (self.SP & 0xFF) as u8;
                self.MMU.mmap[(addr + 1) as usize] = (self.SP >> 8) as u8;
            }
            Ops::inc_r16(d) => {
                match d {
                    Registers_16::AF => panic!(),
                    Registers_16::BC => self.BC += 1,
                    Registers_16::DE => self.DE += 1,
                    Registers_16::HL => self.HL += 1,
                    Registers_16::SP => self.SP += 1,
                    Registers_16::HLI => panic!(),
                    Registers_16::HLD => panic!(),
                };
            }
            Ops::dec_r16(d) => {
                match d {
                    Registers_16::AF => panic!(),
                    Registers_16::BC => self.BC -= 1,
                    Registers_16::DE => self.DE -= 1,
                    Registers_16::HL => self.HL -= 1,
                    Registers_16::SP => self.SP -= 1,
                    Registers_16::HLI => panic!(),
                    Registers_16::HLD => panic!(),
                };
            }
            Ops::add_hl_r16(s) => {
                let r = match s {
                    Registers_16::AF => panic!(),
                    Registers_16::BC => self.BC,
                    Registers_16::DE => self.DE,
                    Registers_16::HL => self.HL,
                    Registers_16::SP => self.SP,
                    Registers_16::HLI => panic!(),
                    Registers_16::HLD => panic!(),
                };

                self.set_sub_flag(false);
                self.set_half_carry_flag((((self.HL & 0xFFF) + (r & 0xFFF)) & 0x1000) == 0x1000);
                self.set_carry_flag(self.HL + r < r || self.HL + r < self.HL);

                self.HL += r;
            }
            Ops::inc_r8(d) => match d {
                Registers_8::A => {
                    let mut R = (self.AF >> 8) as u8;
                    self.set_half_carry_flag(((R & 0b111) + 1) & 0b1000 == 0b1000);
                    R += 1;
                    self.set_zero_flag(R == 0);
                    self.set_sub_flag(false);
                    self.AF &= 0xFF;
                    self.AF |= (R as u16) << 8;
                }
                Registers_8::B => {
                    let mut R = (self.BC >> 8) as u8;
                    self.set_half_carry_flag(((R & 0b111) + 1) & 0b1000 == 0b1000);
                    R += 1;
                    self.set_zero_flag(R == 0);
                    self.set_sub_flag(false);
                    self.AF &= 0xFF;
                    self.AF |= (R as u16) << 8;
                }
                Registers_8::C => {
                    let mut R = (self.BC & 0xFF) as u8;
                    self.set_half_carry_flag(((R & 0b111) + 1) & 0b1000 == 0b1000);
                    R += 1;
                    self.set_zero_flag(R == 0);
                    self.set_sub_flag(false);
                    self.BC &= !(0xFF as u16);
                    self.BC |= R as u16;
                }
                Registers_8::D => {
                    let mut R = (self.DE >> 8) as u8;
                    self.set_half_carry_flag(((R & 0b111) + 1) & 0b1000 == 0b1000);
                    R += 1;
                    self.set_zero_flag(R == 0);
                    self.set_sub_flag(false);
                    self.AF &= 0xFF;
                    self.AF |= (R as u16) << 8;
                }
                Registers_8::E => {
                    let mut R = (self.DE & 0xFF) as u8;
                    self.set_half_carry_flag(((R & 0b111) + 1) & 0b1000 == 0b1000);
                    R += 1;
                    self.set_zero_flag(R == 0);
                    self.set_sub_flag(false);
                    self.DE &= !(0xFF as u16);
                    self.DE |= R as u16;
                }
                Registers_8::H => {
                    let mut R = (self.HL >> 8) as u8;
                    self.set_half_carry_flag(((R & 0b111) + 1) & 0b1000 == 0b1000);
                    R += 1;
                    self.set_zero_flag(R == 0);
                    self.set_sub_flag(false);
                    self.AF &= 0xFF;
                    self.AF |= (R as u16) << 8;
                }
                Registers_8::L => {
                    let mut R = (self.HL >> 8) as u8;
                    self.set_half_carry_flag(((R & 0b111) + 1) & 0b1000 == 0b1000);
                    R += 1;
                    self.set_zero_flag(R == 0);
                    self.set_sub_flag(false);
                    self.HL &= 0xFF;
                    self.HL |= (R as u16) << 8;
                }
                Registers_8::HL => {
                    let mut R = self.MMU.mmap[self.HL as usize];
                    self.set_half_carry_flag(((R & 0b111) + 1) & 0b1000 == 0b1000);
                    R += 1;
                    self.set_zero_flag(R == 0);
                    self.set_sub_flag(false);
                    self.MMU.mmap[self.HL as usize] = R;
                }
            },
            Ops::dec_r8(d) => match d {
                Registers_8::A => {
                    let mut R = (self.AF >> 8) as u8;
                    self.set_half_carry_flag(((R & 0b111) - 1) & 0b1000 == 0b1000);
                    R -= 1;
                    self.set_zero_flag(R == 0);
                    self.set_sub_flag(false);
                    self.AF &= 0xFF;
                    self.AF |= (R as u16) << 8;
                }
                Registers_8::B => {
                    let mut R = (self.BC >> 8) as u8;
                    self.set_half_carry_flag(((R & 0b111) - 1) & 0b1000 == 0b1000);
                    R -= 1;
                    self.set_zero_flag(R == 0);
                    self.set_sub_flag(false);
                    self.AF &= 0xFF;
                    self.AF |= (R as u16) << 8;
                }
                Registers_8::C => {
                    let mut R = (self.BC & 0xFF) as u8;
                    self.set_half_carry_flag(((R & 0b111) - 1) & 0b1000 == 0b1000);
                    R -= 1;
                    self.set_zero_flag(R == 0);
                    self.set_sub_flag(false);
                    self.BC &= !(0xFF as u16);
                    self.BC |= R as u16;
                }
                Registers_8::D => {
                    let mut R = (self.DE >> 8) as u8;
                    self.set_half_carry_flag(((R & 0b111) - 1) & 0b1000 == 0b1000);
                    R -= 1;
                    self.set_zero_flag(R == 0);
                    self.set_sub_flag(false);
                    self.AF &= 0xFF;
                    self.AF |= (R as u16) << 8;
                }
                Registers_8::E => {
                    let mut R = (self.DE & 0xFF) as u8;
                    self.set_half_carry_flag(((R & 0b111) - 1) & 0b1000 == 0b1000);
                    R -= 1;
                    self.set_zero_flag(R == 0);
                    self.set_sub_flag(false);
                    self.DE &= !(0xFF as u16);
                    self.DE |= R as u16;
                }
                Registers_8::H => {
                    let mut R = (self.HL >> 8) as u8;
                    self.set_half_carry_flag(((R & 0b111) - 1) & 0b1000 == 0b1000);
                    R -= 1;
                    self.set_zero_flag(R == 0);
                    self.set_sub_flag(false);
                    self.AF &= 0xFF;
                    self.AF |= (R as u16) << 8;
                }
                Registers_8::L => {
                    let mut R = (self.HL >> 8) as u8;
                    self.set_half_carry_flag(((R & 0b111) - 1) & 0b1000 == 0b1000);
                    R -= 1;
                    self.set_zero_flag(R == 0);
                    self.set_sub_flag(false);
                    self.HL &= 0xFF;
                    self.HL |= (R as u16) << 8;
                }
                Registers_8::HL => {
                    let mut R = self.MMU.mmap[self.HL as usize];
                    self.set_half_carry_flag(((R & 0b111) - 1) & 0b1000 == 0b1000);
                    R -= 1;
                    self.set_zero_flag(R == 0);
                    self.set_sub_flag(false);
                    self.MMU.mmap[self.HL as usize] = R;
                }
            },
            Ops::ld_r8_imm8(d) => {
                let byte = self.read_next_byte();
                match d {
                    Registers_8::A => {
                        self.AF &= 0x00FF;
                        self.AF |= (byte as u16) << 8;
                    }
                    Registers_8::B => {
                        self.BC &= 0x00FF;
                        self.BC |= (byte as u16) << 8;
                    }
                    Registers_8::C => {
                        self.BC &= 0xFF00;
                        self.BC |= byte as u16;
                    }
                    Registers_8::D => {
                        self.DE &= 0x00FF;
                        self.DE |= (byte as u16) << 8;
                    }
                    Registers_8::E => {
                        self.DE &= 0xFF00;
                        self.DE |= byte as u16;
                    }
                    Registers_8::H => {
                        self.HL &= 0x00FF;
                        self.HL |= (byte as u16) << 8;
                    }
                    Registers_8::L => {
                        self.AF &= 0xFF00;
                        self.AF |= byte as u16;
                    }
                    Registers_8::HL => {
                        self.MMU.mmap[self.HL as usize] = self.read_next_byte();
                    }
                }
            }
            Ops::rlca => {
                let mut A = (self.AF >> 8) as u8;
                let b0 = A >> 7;
                A = (A << 1) | b0;
                self.AF &= 0x00FF;
                self.AF |= (A as u16) << 8;

                self.set_carry_flag(b0 != 0);
                self.set_half_carry_flag(false);
                self.set_sub_flag(false);
                self.set_zero_flag(false);
            }
            Ops::rrca => {
                let mut A = (self.AF >> 8) as u8;
                let b7 = A << 7;
                A = (A >> 1) | b7;
                self.AF &= 0x00FF;
                self.AF |= (A as u16) << 8;

                self.set_carry_flag(b7 != 0);
                self.set_half_carry_flag(false);
                self.set_sub_flag(false);
                self.set_zero_flag(false);
            }
            Ops::rla => {
                let mut A = (self.AF >> 8) as u8;
                let c = ((self.AF & 0b0001_0000) >> 4) as u8;
                self.set_carry_flag((A >> 7) != 0);
                A = (A << 1) | c;

                self.AF &= 0x00FF;
                self.AF |= (A as u16) << 8;
            }
            Ops::rra => {
                let mut A = (self.AF >> 8) as u8;
                let c = ((self.AF & 0b0001_0000) >> 4) as u8;
                self.set_carry_flag((A & 1) != 0);
                A = (A >> 1) | c << 7;

                self.AF &= 0x00FF;
                self.AF |= (A as u16) << 8;
            }
            Ops::daa => todo!(),
            Ops::cpl => {
                let A = !((self.AF >> 8) as u8);
                self.AF &= 0x00FF;
                self.AF |= (A as u16) << 8;
            }
            Ops::scf => self.set_carry_flag(true),
            Ops::ccf => {
                let c = (self.AF & 0b0001_0000) >> 4;
                self.set_carry_flag(c == 0);
            }
            Ops::jr_imm8 => todo!(),
            Ops::jr_cond_imm8(c) => todo!(),
            Ops::stop => todo!(),
            Ops::ld_r8_r8(d, s) => {
                let (s, os) = match s {
                    Registers_8::A => (&self.AF, 8),
                    Registers_8::B => (&self.BC, 8),
                    Registers_8::C => (&self.BC, 0),
                    Registers_8::D => (&self.DE, 8),
                    Registers_8::E => (&self.DE, 0),
                    Registers_8::H => (&self.HL, 8),
                    Registers_8::L => (&self.HL, 0),
                    Registers_8::HL => panic!(),
                };

                let byte = (*s >> os) as u8;

                let (d, od) = match d {
                    Registers_8::A => (&mut self.AF, 8),
                    Registers_8::B => (&mut self.BC, 8),
                    Registers_8::C => (&mut self.BC, 0),
                    Registers_8::D => (&mut self.DE, 8),
                    Registers_8::E => (&mut self.DE, 0),
                    Registers_8::H => (&mut self.HL, 8),
                    Registers_8::L => (&mut self.HL, 0),
                    Registers_8::HL => panic!(),
                };

                *d = *d & !(0xFF << od);
                *d = *d | (byte << od) as u16;
            }
            Ops::halt => todo!(),
            Ops::add_a_r8(s) => {
                let (s, os) = match s {
                    Registers_8::A => (&self.AF, 8),
                    Registers_8::B => (&self.BC, 8),
                    Registers_8::C => (&self.BC, 0),
                    Registers_8::D => (&self.DE, 8),
                    Registers_8::E => (&self.DE, 0),
                    Registers_8::H => (&self.HL, 8),
                    Registers_8::L => (&self.HL, 0),
                    Registers_8::HL => panic!(),
                };

                let byte = (*s >> os) as u8;

                self.AF &= 0x00FF;
                self.AF |= (byte as u16) << 8;
            }
            Ops::adc_a_r8(s) => todo!(),
            Ops::sub_a_r8(s) => todo!(),
            Ops::sbc_a_r8(s) => todo!(),
            Ops::and_a_r8(s) => todo!(),
            Ops::xor_a_r8(s) => todo!(),
            Ops::or_a_r8(s) => todo!(),
            Ops::cp_a_r8(s) => todo!(),
            Ops::add_a_imm8 => todo!(),
            Ops::adc_a_imm8 => todo!(),
            Ops::sub_a_imm8 => todo!(),
            Ops::sbc_a_imm8 => todo!(),
            Ops::and_a_imm8 => todo!(),
            Ops::xor_a_imm8 => todo!(),
            Ops::or_a_imm8 => todo!(),
            Ops::cp_a_imm8 => todo!(),
            Ops::ret_cond(c) => todo!(),
            Ops::ret => todo!(),
            Ops::reti => todo!(),
            Ops::jp_cond_imm16(c) => todo!(),
            Ops::jp_imm16 => todo!(),
            Ops::jp_hl => todo!(),
            Ops::call_cond_imm16(c) => todo!(),
            Ops::call_imm16 => todo!(),
            Ops::rst_tgt3() => todo!(),
            Ops::pop_r16stk(d) => todo!(),
            Ops::push_r16stk(d) => todo!(),
            Ops::ldh_c_a => todo!(),
            Ops::ldh_imm8_a => todo!(),
            Ops::ld_imm16_a => todo!(),
            Ops::ldh_a_c => todo!(),
            Ops::ldh_a_imm8 => todo!(),
            Ops::ld_a_imm16 => todo!(),
            Ops::add_sp_imm8 => todo!(),
            Ops::ld_hl_sp_imm8 => todo!(),
            Ops::ld_sp_hl => todo!(),
            Ops::di => todo!(),
            Ops::ei => todo!(),
            Ops::prefix => todo!(),
            Ops::rlc_r8(r) => todo!(),
            Ops::rrc_r8(r) => todo!(),
            Ops::rl_r8(r) => todo!(),
            Ops::rr_r8(r) => todo!(),
            Ops::sla_r8(r) => todo!(),
            Ops::sra_r8(r) => todo!(),
            Ops::swap_r8(r) => todo!(),
            Ops::srl_r8(r) => todo!(),
            Ops::bit_b3_r8(b, r) => todo!(),
            Ops::res_b3_r8(b, r) => todo!(),
            Ops::set_b3_r8(b, r) => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_flag() {
        let mut cpu = CPU::init();
        let flags = 0b0101_1010;
        cpu.set_flags(flags);
    }
}
