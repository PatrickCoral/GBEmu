pub struct MMU {
    mmap: [u8; 0xFFFF],
}

enum MemoryType {
    CARTRIDGE = 0x0000,
    VRAM = 0x8000,
    WRAM = 0xC000,
}

impl MMU {
    fn new() -> Self {
        Self { mmap: [0; 0xFFFF] }
    }

    fn writeTo(&mut self, memType: MemoryType, offset: u16, word: u8) {
        let base = (memType as u16 + offset) as usize;

        self.mmap[base] = word;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_mem() {
        let mmu = MMU::new();
		let mut memType = MemoryType::VRAM;
		assert_eq!(format!("{:#X}", memType as u16), "0x8000");
		memType = MemoryType::WRAM;
		println!("{:#X}", memType as u16);
    }
}
