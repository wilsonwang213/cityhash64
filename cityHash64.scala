import java.nio.charset.StandardCharsets
object MyClass {
    val k0 = 0xc3a5c85c97cb3127L
val k1 = 0xb492b66fbe98f273L
val k2 = 0x9ae16a3b2f90404fL
val k3 = 0xc949d7c7509e6557L
val k5 = 0x9ddfea08eb382d69L

def toLongLE(b: Array[Byte], i: Int):Long = {
    0xffffffffffffffffL & ((b(i + 7).toLong << 56) + ((b(i + 6) & 255).toLong << 48) + ((b(i + 5) & 255).toLong << 40) + ((b(i + 4) & 255).toLong << 32) + ((b(i + 3) & 255).toLong << 24) + ((b(i + 2) & 255) << 16) + ((b(i + 1) & 255) << 8) + ((b(i + 0) & 255) << 0))
}

def toIntLE(b: Array[Byte], i: Int):Long = {
    0xffffffffL & (((b(i + 3) & 255) << 24) + ((b(i + 2) & 255) << 16) + ((b(i + 1) & 255) << 8) + ((b(i + 0) & 255) << 0))
}

def Fetch64(p: Array[Byte], index: Int):Long = {
    toLongLE(p, index)
}

def Fetch32(p: Array[Byte], index: Int):Long = {
    toIntLE(p, index)
}

def Rotate(value: Long, shift: Int) = {
    if (shift == 0) value else (value >>> shift) | (value << (64 - shift))
}

def ShiftMix(value: Long) = {
    value ^ (value >>> 47)
}

def Uint128Low64(x: Array[Long]) = {
    x(0)
}

def Uint128High64(x: Array[Long]) = {
    x(1)
}

def Hash128to64(x: Array[Long]) = {
    var a = (Uint128Low64(x) ^ Uint128High64(x)) * k5
    a ^= (a >>> 47)
    var b = (Uint128High64(x) ^ a) * k5
    b ^= (b >>> 47)
    b *= k5
    b
}

def HashLen16(u: Long, v: Long) = {
    Hash128to64(Array(u,v))
}

def HashLen17to32(s: Array[Byte], index: Int, len: Int) = {
    var a = Fetch64(s, index) * k1
    var b = Fetch64(s, index + 8)
    var c = Fetch64(s, index + len - 8) * k2
    var d = Fetch64(s, index + len - 16) * k0
    HashLen16(Rotate(a - b, 43) + Rotate(c, 30) + d, a + Rotate(b ^ k3, 20) - c + len)
}

def RotateByAtLeastOne(value: Long, shift: Int) = {
    (value >>> shift) | (value << (64 - shift))
}

def HashLen0to16(s: Array[Byte], index: Int, len: Int) = {
    if (len > 8) {
        var a = Fetch64(s, index)
        var b = Fetch64(s, index + len - 8)
        HashLen16(a, RotateByAtLeastOne(b + len, len)) ^ b
    }
    if (len >= 4) {
        var a = Fetch32(s, index)
        HashLen16(len + (a << 3), Fetch32(s, index + len - 4))
    }
    if (len > 0) {
        val a = s(index)
        val b = s(index + len >>> 1)
        val c = s(index + len - 1)
        val y = (a) + (b << 8)
        val z = len + (c << 2)
        ShiftMix(y * k2 ^ z * k3) * k2
    }
    k2
}

def HashLen33to64(s: Array[Byte], index: Int, len: Int) = {
    var z = Fetch64(s, index + 24)
    var a = Fetch64(s, index) + (len + Fetch64(s, index + len - 16)) * k0
    var b = Rotate(a + z, 52)
    var c = Rotate(a, 37)
    a += Fetch64(s, index + 8)
    c += Rotate(a, 7)
    a += Fetch64(s, index + 16)
    var vf = a + z
    var vs = b + Rotate(a, 31) + c
    a = Fetch64(s, index + 16) + Fetch64(s, index + len - 32)
    z = Fetch64(s, index + len - 8)
    b = Rotate(a + z, 52)
    c = Rotate(a, 37)
    a += Fetch64(s, index + len - 24)
    c += Rotate(a, 7)
    a += Fetch64(s, index + len - 16)
    var wf = a + z
    var ws = b + Rotate(a, 31) + c
    var r = ShiftMix((vf + ws) * k2 + (wf + vs) * k0)
    ShiftMix(r * k0 + vs) * k2
}

def WeakHashLen32WithSeedsRecursive(w: Long, x: Long, y: Long, z: Long, a: Long, b: Long):Array[Long] = {
    var aa = a + w
    var bb = Rotate(b + a + z, 21)
    val c = aa
    aa += x
    aa += y
    bb += Rotate(a, 44)
    Array(aa + z, bb + c)
}

def WeakHashLen32WithSeeds(s: Array[Byte], index: Int, a: Long, b: Long):Array[Long] = {
    WeakHashLen32WithSeedsRecursive(
        Fetch64(s, index),
        Fetch64(s, index + 8),
        Fetch64(s, index + 16),
        Fetch64(s, index + 24),
        a,
        b
    )
}

def CityHash64(s: Array[Byte], index: Int, len: Int):Long = {
    if (len <= 16 ) {
        HashLen0to16(s, index, len)
    } else if (len > 16 && len <= 32) {
        HashLen17to32(s, index, len)
    } else if (len > 32 && len <= 64) {
        HashLen33to64(s, index, len)
    } else {
        var x = Fetch64(s, index)
        var y = Fetch64(s, index + len - 16) ^ k1
        var z = Fetch64(s, index + len - 56) ^ k0
        var v = WeakHashLen32WithSeeds(s, len - 64, len, y)
        var w = WeakHashLen32WithSeeds(s, len - 32, len * k1, k0)
        z += ShiftMix(v(1)) * k1
        x = Rotate(z + x, 39) * k1
        y = Rotate(y, 33) * k1

        var length = (len - 1) & ~63
        var idx = index
        do {
            x = Rotate(x + y + v(0) + Fetch64(s, idx + 16), 37) * k1
            y = Rotate(y + v(1) + Fetch64(s, idx + 48), 42) * k1
            x ^= w(1)
            y ^= v(0)
            z = Rotate(z ^ w(0), 33)
            v = WeakHashLen32WithSeeds(s, idx, v(1) * k1, x + w(0))
            w = WeakHashLen32WithSeeds(s, idx + 32, z + w(1), y)
            var t = z
            z = x
            x = t
            idx += 64
            length -= 64
        } while (length != 0)
        HashLen16(HashLen16(v(0), w(0)) + ShiftMix(y) * k1 + z, HashLen16(v(1), w(1)) + x)
    }
}


def CHCityHash64(strs: Seq[String]) = {
    val bytes = strs.mkString.getBytes()
    java.lang.Long.toUnsignedString(CityHash64(bytes, 0, bytes.length), 10)
}
    def main(args: Array[String]) {

        print("Hash = " + CHCityHash64(Seq("John")));
    }
}