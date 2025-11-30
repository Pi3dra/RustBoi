ROM_FILE = "dmg-acid2.gb" 

with open(ROM_FILE, "rb") as f:
    rom = f.read()

rom_size = len(rom)
dump_size = min(32 * 1024, rom_size)  # 32 KB or ROM size if smaller

# Dump in hex table format
for addr in range(0, dump_size, 16):
    chunk = rom[addr:addr+16]
    hex_bytes = ' '.join(f'{b:02X}' for b in chunk)
    print(f"{addr:04X}: {hex_bytes}")
