# Demo

1. Create C++ project
    12KiB release mode, not bad?
2.  Need static linking
    200KiB, less good
3.  x64 too big! x86 better!
    156KiB, less good

## Commands

```bash
# Minifies shader into "readable" minified shader code
..\shader_minifier.exe .\shader.fx --format indented -o shader_mini.fx

# Minifies shader into "unreadable" minified shader code
..\shader_minifier.exe .\shader.fx --format c-array -o shader.inl

# Generate assembler code from sointu song file
..\sointu-compile.exe -arch 386 -e "h,asm" music-sointu.yml
```
## Externals

1. PPPE - To inspect files - https://www.mzrst.com/
2. Crinkler - Replacement linker to produce compact binaries - https://github.com/runestubbe/Crinkler
3. Shader minifier - Minifies shader code - https://github.com/laurentlb/Shader_Minifier
4. Sointu - Produces music for compact binaries - https://github.com/vsariola/sointu
5. 4Klang - Produces music for compact binaries - https://github.com/hzdgopher/4klang



..\shader_minifier.exe .\shader.fx -o shader.h