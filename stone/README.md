# Stone

## How to build

### First stage

Use external assemblers (like `nasm`) to build a first-stage binary.

```
nasm -f bin self.asm -o stone-0
chmod +x stone-0
```

### Second state

Use a first-stage build to build itself.

```
./stone-0 < self.asm 2> stone
chmod +x stone
```

(Compilation will take time..., I will optimize it in later)
