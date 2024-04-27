#[allow(non_snake_case)]

pub(crate) enum Registers_16 {
    BC,
    DE,
    HL,
}

pub(crate) enum Registers_8 {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
}

pub(crate) enum Conditions {
    NZ,
    Z,
    NC,
    C,
}

pub(crate) struct CPU {
    AF: u16,
    BD: u16,
    DE: u16,
    HL: u16,
    SP: u16,
    PC: u16,
}

impl CPU {
    pub(crate) fn init() -> Self {
        Self {
            AF: 0,
            BD: 0,
            DE: 0,
            HL: 0,
            SP: 0,
            PC: 0,
        }
    }

    pub(crate) fn set_flags(&mut self, flags: u8) {
        let flags = (flags & 0b1111_0000) as u16;
        let mask = !(0b1111_0000 as u16);
        self.AF &= mask;
        self.AF |= flags;
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
