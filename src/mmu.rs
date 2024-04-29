use std::fs::read;

pub struct MMU {
    mmap: [u8; 0xFFFF],
}

enum MemoryType {
    BANK_0 = 0x0000,
    BANK_N = 0x4000,
    VRAM = 0x8000,
    WRAM = 0xC000,
}

impl MMU {
    pub(crate) fn new() -> Self {
        Self { mmap: [0; 0xFFFF] }
    }

    fn writeTo(&mut self, memType: MemoryType, offset: u16, word: u8) {
        let base = (memType as u16 + offset) as usize;

        self.mmap[base] = word;
    }

    pub(crate) fn load_bank_0(&mut self, path: &'static str) -> Result<(), ()> {
        let file = read(path);
        match file {
            Ok(bytes) => {
                println!("{}", bytes.len());
                for i in 0x0000..0x4000 {
                    self.writeTo(MemoryType::BANK_0, i as u16, bytes[i]);
                }
                Ok(())
            }
            Err(_) => Err(()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_mem() {
        let mut mmu = MMU::new();
        let mut memType = MemoryType::VRAM;
        assert_eq!(format!("{:#X}", memType as u16), "0x8000");
        memType = MemoryType::WRAM;
        println!("{:#X}", memType as u16);
        assert!(mmu.load_bank_0("/home/patrick/Documents/rust/GBEmu/res/cpu_instrs.gb").is_ok());
    }
}
