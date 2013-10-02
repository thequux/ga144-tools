package assembler

import (
	"fmt"
	"github.com/golang/glog"
	"io"
)

type Assembler struct {
	line, col int
	pass      int
	symtab    map[string]int
	loc       int
	addrspace [512]int
	touched   [512]bool
	insnp     [512]bool
	partial   bool
	curOp     uint
	errors    int
	deferred  []int // Data to be emitted when we flush the current word
	locUsed   bool  // true if .loc held valid data when we first pointed to it.
}

var InsnNames = []string{
	";",
	"ex",
	";name",
	"name",
	"unext",
	"next",
	"if",
	"-if",
	"@p", "@+", "@b", "@",
	"!p", "!+", "!b", "!",
	"+*", "2*", "2/", "~",
	"+", "and", "xor", "drop",
	"dup", "pop", "over", "a",
	"nop", "push", "b!", "a!"}

func (asm *Assembler) Init() {
	asm.symtab = map[string]int{}
	for x := 0; x < 512; x++ {
		asm.symtab[fmt.Sprintf("_%02x_", x)] = x
	}
}

func (asm *Assembler) PrePass() {
	glog.V(1).Infof("Beginning pass %d", asm.pass)
	asm.deferred = make([]int, 0, 4)
	for i := range asm.addrspace {
		asm.addrspace[i] = 0
		asm.touched[i] = false
		asm.insnp[i] = false
	}
	asm.loc = 0
	asm.curOp = 0
}

func (asm *Assembler) emit(opcode int, name string) {
	glog.V(2).Infof("emit opcode %d", opcode)
	// opcodes that take an argument
	var dest int
	var ok bool
	if opcode%4 != 0 && asm.curOp == 3 {
		glog.Fatalf("Tried to use long instruction %s(%s) in slot 3", opcode, InsnNames[opcode])
	}

	asm.partial = true

	// add opcode to word...
	opcBits := (0x1F << ((3 - asm.curOp) * 5)) >> 2;
	asm.addrspace[asm.loc] |= (opcode << ((3 - asm.curOp) * 5)) >> 2
	asm.addrspace[asm.loc] ^= opcBits & 0x15555
	asm.touched[asm.loc] = true
	asm.curOp++
	if opcode >= 2 && opcode <= 7 && opcode != 4 {
		// finsihes word
		destBits := (3-asm.curOp)*5 + 3 // number of bits available for destination
		destMask := (1 << destBits) - 1
		if asm.pass > 0 {
			// Symbol value is known
			dest, ok = asm.symtab[name]
			if !ok {
				asm.Error("Unresolved identifier %s", name)
			}
			if (dest &^ destMask) != (asm.loc &^ destMask) {
				asm.Error("Not enough space for jump target in word (jump to %s)", name)
			}
		}
		asm.addrspace[asm.loc] |= dest & destMask

		asm.curOp = 4 // finish word
		asm.flushWord()
		return
	}

	if asm.curOp == 4 {
		asm.flushWord()
	}
}

func (asm *Assembler) at(loc int) {
	glog.V(2).Infof("Setting loc")
	// word is already flushed
	if loc&0x40 == 0x40 && loc < 0x100 {
		loc -= 0x40 // keep addresses small.
	}
	asm.loc = loc
	asm.curOp = 0
	if asm.touched[loc] {
		asm.locUsed = true
	}
}

func (asm *Assembler) flushWord() {
	glog.V(2).Infof("Tried to flush %d opcodes", asm.curOp)

	if asm.curOp == 4 {
		asm.at(asm.loc + 1)
	} else if asm.curOp > 0 && asm.curOp < 4 {
		curLoc := asm.loc
		for asm.loc == curLoc {
			asm.emit(28, "") // nop
		}
		return // will end up calling back into this func, so
		// the following processing is already done.
	}

	glog.V(2).Infof("Dumping %d deferreds", len(asm.deferred))
	for _, v := range asm.deferred {
		if asm.locUsed {
			asm.Error("Memory conflict at 0x%x", asm.loc)
		}
		asm.addrspace[asm.loc] = v
		asm.touched[asm.loc] = true
		asm.at(asm.loc + 1)
	}
	asm.deferred = asm.deferred[0:0]
}

func (asm *Assembler) defName(name string) {
	glog.V(1).Infof("Defining %q at offset 0x%x.%d", name, asm.loc, asm.curOp)

	if pos, ok := asm.symtab[name]; ok && pos != asm.loc {
		asm.Error("Label %q multiply defined", name)
	}
	asm.symtab[name] = asm.loc
}

func (asm *Assembler) enqueue(lit int) {
	asm.deferred = append(asm.deferred, lit)
	if asm.curOp == 0 {
		asm.flushWord()
	}
	glog.V(2).Infof("Deferred %v; %v items deferred", lit, len(asm.deferred))
}

func (asm *Assembler) Error(format string, args ...interface{}) {
	glog.Infof(fmt.Sprintf("%d: %s", asm.line, format), args...)
	asm.errors++
}

func (asm Assembler) Dump(w io.Writer) {
	for i, val := range asm.addrspace {
		mask := 0
		if asm.touched[i] {
			fmt.Fprintf(w, "%02x: %05x\n", i, val^mask)
		}
	}
}
