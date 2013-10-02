package main

import (
	"flag"
	"github.com/golang/glog"
	"gopkg.thequux.com/ga144/assembler"
	"io/ioutil"
	"os"
)

func main() {
	flag.Parse()

	var data []byte
	var err error
	if data, err = ioutil.ReadAll(os.Stdin); err != nil {
		glog.Fatalf("Failed to read input: %s", err)
	}
	asm := assembler.Assembler{}
	asm.Init()
	asm.RunPass(data)
	asm.RunPass(data)

	asm.Dump(os.Stdout)
}