# RustBoi

This is a **GameBoy** emulator written in rust!
![Demo GIF](/test_roms/Tetris.gif)


It uses the **Libretro API** to run on the **RetroArch** frontend.
https://www.retroarch.com/

**RetroArch** can be quickly installed like so: 

Debian/Ubuntu
```
sudo add-apt-repository ppa:libretro/stable
sudo apt update
sudo apt install retroarch
```

Arch/Manjaro
```
sudo pacman -S retroarch
sudo pacman -S libretro-*
```

It can also easily be installed trhough Flatpak!

## Running

The emulator can easily be ran like so:

After building with `cargo build --release`:

```
retroarch -v -L target/release/librust_boi.so <path_to_rom>
```

There are some quick `test_roms`

- blargs cpu_instrs tests (for cpu instructions)
- dmg-acid2.gn (for ppu)
- buttontest.gb

## Missing features and bugs

### Not Implemented yet

- Sound 
- MBC banking, can thus only run MBC0 games i.e 32Kb games
- Timer and Timer Interrupts (I attempted to implement this, but is severely bugged, documentation is rather obscure)
- I want to implement a debugger to display memory and do step by step execution.

### bugs

Some small ppu bugs in dmg-acid2, first object sprite seems to be shifted to the right

In buttontest.gb weirdly when the ppu is running at the "correct" cycle speed (i.e 4 times as those as the CPU) this rom stops working.

Opus5 and Boxxle bug just after the start, the problem is probably missing interrupts for handling animations and other stuff.


