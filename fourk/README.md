# Demo

## Preparation

1. Goto: https://www.youtube.com/watch?v=cDuUmCQJdVc
1.1. So we don't have to watch ads during presenation
2. Open Windows terminal in FourK folder
3. Clear out residues from previous runs
4. Start Visual Studio
5. Start Visual Studio Code

## Script

1. Show HelloWorld and the new to drop CRT
2. Introduce a minimal windows app
3. Compile with Crinkler
4. Minify with Shader minifier
5. Inline code
6. Show sointu
7. Show minesweeper


## Commands

```bash
# Minifies shader into "readable" minified shader code
..\shader_minifier.exe .\shader.fx --format indented -o shader_mini.fx

# Minifies shader into "unreadable" minified shader code
..\shader_minifier.exe .\shader.fx --format c-array -o shader.inl

# Generate assembler code from sointu song file
..\sointu-compile.exe -arch 386 -e "h,asm" uglyverse.yml
```
## Externals

1. PPPE - To inspect files - https://www.mzrst.com/
2. Crinkler - Replacement linker to produce compact binaries - https://github.com/runestubbe/Crinkler
3. Shader minifier - Minifies shader code - https://github.com/laurentlb/Shader_Minifier
4. Sointu - Produces music for compact binaries - https://github.com/vsariola/sointu
5. 4Klang - Produces music for compact binaries - https://github.com/hzdgopher/4klang



..\shader_minifier.exe .\shader.fx -o shader.h