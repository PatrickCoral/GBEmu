const BANK_SIZE: u16 = 0x4000;

#[allow(non_camel_case_types)]
enum CartridgeType {
    ROM_ONLY,
    MBC1,
    MBC1_RAM,
    MBC1_RAM_BATTERY,
    MBC2,
    MBC2_BATTERY,
    ROM_RAM,
    ROM_RAM_BATTERY,
    MMM01,
    MMM01_RAM,
    MMM01_RAM_BATTERY,
    MBC3_TIMER_BATTERY,
    MBC3_TIMER_RAM_BATTERY,
    MBC3,
    MBC3_RAM,
    MBC3_RAM_BATTERY,
    MBC5,
    MBC5_RAM,
    MBC5_RAM_BATTERY,
    MBC5_RUMBLE,
    MBC5_RUMBLE_RAM,
    MBC5_RUMBLE_RAM_BATTERY,
    MBC6,
    MBC7_SENSOR_RUMBLE_RAM_BATTERY,
    POCKET_CAMERA,
    BANDAI_TAMA5,
    HuC3,
    HuC1_RAM_BATTERY,
}

pub struct MMU {
    pub mmap: [u8; 0xFFFF],
    bank_addr: u16,
    rom_banks: u8,
    ram_banks: u8,
}

#[allow(non_camel_case_types)]
enum MemoryType {
    CARTRIDGE = 0x0000,
    BANK_N = 0x4000,
    VRAM = 0x8000,
    WRAM = 0xC000,
    IO_REG = 0xFF00,
    HRAM = 0xFF80,
    INT_REG = 0xFFFF,
}

impl MMU {
    pub(crate) fn init() -> Self {
        let mut mmu = Self {
            mmap: [0; 0xFFFF],
            bank_addr: BANK_SIZE,
            rom_banks: 2,
            ram_banks: 0,
        };
        mmu.mmap[0xFF50] = 1;
        mmu
    }

    fn writeTo(&mut self, memType: MemoryType, offset: u16, word: u8) {
        let base = (memType as u16 + offset) as usize;

        self.mmap[base] = word;
    }

    pub(crate) fn switch_bank(&mut self, bank_n: u16) {
        self.bank_addr = bank_n * BANK_SIZE;
    }

    pub(crate) fn read_headers(&mut self) {
        self.rom_banks = self.get_rom_banks();
        self.ram_banks = self.get_ram_banks();
    }

    fn get_rom_banks(&self) -> u8 {
        0b10 << self.mmap[0x0148]
    }

    fn get_ram_banks(&self) -> u8 {
        match self.mmap[0x149] {
            0x02 => 1,
            0x03 => 4,
            0x04 => 16,
            0x05 => 8,
            _ => 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_mem() {
        let mmu = MMU::init();
        let mut memType = MemoryType::VRAM;
        assert_eq!(format!("{:#X}", memType as u16), "0x8000");
        memType = MemoryType::WRAM;
        println!("{:#X}", memType as u16);
    }
}
