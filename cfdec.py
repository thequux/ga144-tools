#!/usr/bin/python

# This code is utter crap; that said, it's intended to be a quick and dirty hack.
# To dump JSON from a Colorforth image:
# - Load the image into colorforth
# - type '0 !back '
# - At your terminal, run "python cfdec.py <OkadBack.cf >Okad.json"
# - cat =(echo -n 'OkadSrc = ') Okad.json =(echo ';') >okad-src.js
#
# On Windows, you will find OkadBack.cf in C:\GreenArrays\EVB001\OkadBack.cf
# On Linux, it's in ~/.wine/dosdevices/c:/GreenArrays/EVB001/OkadBack.cf
# On OSX, you're on your own.

import struct

CHARSET = {
    4: " rtoeani",
    5: "smcylgfw",
    7: ("dvpbhxuq"
        "01234567"
        "89j-k.z/"
        ";'!+@*,?")
}
ANSIFMT = {
    "ext": "",
    "yellow": " \x1b[1;33m",
    "red": " \x1b[1;31m",
    "green": " \x1b[1;32m",
    "cyan": " \x1b[1;36m",
    "white": " \x1b[1;37m",
    "magenta": " \x1b[1;35m",
    "err": "\x1b[1;41;37m",
    "blue": "\x1b[1;34m"
}

def SplitInto(stream, sz):
    pos = 0
    while len(stream) > pos:
        yield stream[pos:pos+sz]
        pos += sz
    # yield stream[pos:]

class IStream(object):
    def __init__(self, fname):
        if type(fname) is tuple:
            self.content = fname[0]
            self.off = fname[1]
        elif type(fname) is file:
            self.off = 0
            self.content = [struct.unpack("<I", x)[0]
                            for x in SplitInto(fname.read(),
                                               4)]
        else:
            IStream.__init__(self, open(fname, 'rb'))
            
    def peek(self):
        if self.off >= len(self.content):
            return 0
        return self.content[self.off]
    def next(self):
        if self.off >= len(self.content):
            return 0
        self.off += 1
        return self.content[self.off -1 ]
    def reset(self):
        self.off = 0

    def __getitem__(self, idx):
        bstart = idx * 256
        return IStream( (self.content[bstart : bstart+256],0) )
    def sub(self, i, l=256):
        return IStream( (self.content[i:i+l], 0) )

def ToBin(v, bits=32):
    # low bits first
    res = ""
    for i in range(bits):
        res = res + ("1" if v & (1 << i) else "0")
    return res

class BitStream(object):
    def __init__(self, val, bits):
        self.bits = bits
        self.val = val
    def peek(self, b):
        if self.bits > 0:
            # Ew. Apparently the word needs to be padded with enough 0
            # bits to fill out the last character
            return ((self.val << b) >> self.bits) % (1 << b)
        else:
            raise StopIteration()
    def bitsel(self, start, count):
        return (self.val >> start) % (1 << count)
    def pop(self, b):
        v = self.peek(b)
        self.bits -= b
        return v
    

def Word2Str(word, bits):
    bs = BitStream(word, bits)
    try:
        while True:
            if bs.pop(1) == 0: # "0"
                v = bs.pop(3)
                #print (bs.bits, 4, CHARSET[4][v])
                yield CHARSET[4][v]
            elif bs.pop(1) == 0: # "10"
                v = bs.pop(3)
                #print (bs.bits, 5, CHARSET[5][v])
                yield CHARSET[5][v]
            else: # "11"
                v = bs.pop(5)
                #print (bs.bits, 7, CHARSET[7][v])
                yield CHARSET[7][v]
    except StopIteration:
        pass

class PWord(object):
    __slots__ = ('name','tag','value')
    def __init__(self, tag, name, value=None):
        self.tag = tag
        self.name = name
        self.value = value

    def asJSON(self):
        if self.value is not None:
            return [self.tag, self.name, self.value]
        else:
            return [self.tag, self.name]
    def __str__(self):
        suffix = ("%s%s" % (ANSIFMT['green'],self.value)) if self.value is not None else ''
        return ANSIFMT[TYPES[self.tag][0]] + str(self.name) + suffix
    
    def __repr__(self):
        return str(self) + "\x1b[0m"

def Word(tag, stream):
    v = stream.next()
    return PWord(tag, "".join(Word2Str(v >> 4, 28)).rstrip())

def Vrbl(tag, stream):
    v = stream.next()
    val = stream.next()
    if val >= 0x80000000:
        val = -0x100000000 + val
    
    return PWord(tag, "".join(Word2Str(v >> 4, 28)).rstrip(), val)

def SNum(tag, stream):
    # TODO: handle hex
    hexp = hex if (stream.peek() & 0x08) == 0 else str
    return PWord(tag, hexp(stream.next() >> 5))
def LNum(tag, stream):
    # TODO: handle hex
    hexp = hex if (stream.peek() & 0x08) == 0 else str
    v = stream.next()
    return PWord(tag, hexp(stream.next()))

def Err(tag, stream):
    return PWord(tag, hex(stream.next()))

TYPES = [
    ("ext",    Word), # extension		0
    ("yellow", Word), # execute			1
    ("yellow", LNum), # exec-num		2
    ("red",    Word), # define			3
    ("green",  Word), # compile			4
    ("green",  LNum), # compile-num		5
    ("green",  SNum), # compile-num		6
    ("cyan",   Word), # compile			7
    
    ("yellow", SNum), # execute			8
    ("white",  Word), # comment			9
    ("white",  Word), # cap comment		a
    ("white",  Word), # all caps comment	b
    ("magenta",Vrbl), # variable		c
    ("err",    Err ), #				d
    ("blue",   Word), #				e
    ("white",  SNum)] #				f

def Destream(stream, merge = True):
    res = []
    while stream.off < len(stream.content):
        tag = stream.peek() & 0xF
        _, fn = TYPES[tag]
        v = fn(tag, stream)
        if tag == 0 and len(res) > 0 and merge:
            res[-1].name = res[-1].name + v.name
        else:
            res.append(v)
    return res
    
def decodeAll(fname):
    result = []
    istream = IStream(fname)
    
    for i in range(1440):
        result.append(Destream(istream[i]))
    return result

def Dump(ostream, data):
    import json
    odata = [map(PWord.asJSON, x) for x in data]
    json.dump(odata, ostream)

if __name__ == '__main__':
    # This requires an uncompressed file, which can be written using ColorForth with "0 !back"; the resulting dump can be found in OkadBack.cf
    import sys
    code = decodeAll( sys.stdin )
    Dump(sys.stdout, code)
    
