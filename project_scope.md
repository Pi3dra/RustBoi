## Project Scope

### Students

- Pedro BARTOLOMEI  `pedro.bartolomei-pandozi@universite-paris-saclay.fr`

### Description
The goal of this project is to write a basic GameBoy emulator in rust, with the following goals:
1. "Easy" Goals
	- Almost full implementation of CPU and Memory 
	- Passing almost all test ROMs from one of these listed repos: https://github.com/c-sp/game-boy-test-roms?tab=readme-ov-file    (my implementations passes blargs cpu_instr inside ../test_roms, excep interrupts as expected)

2. "Medium" Goals (This is what I expect to have at the end of the deadline)
	-  Implement graphics and input
	-  Running  "Simpler" games like Tetris
	- "Enforcing" memory safety and valid input from ROMs

3. "Hard" Goals
	- Getting a proper implementation of hardware interrupts 
	- Try to add already existing shaders to imitate old game boy screen
	- Implementing sound

