

import java.util.Arrays;
import java.util.List;
public class MyClass {

    
     private static final long k0 = 0xc3a5c85c97cb3127L;
    private static final long k1 = 0xb492b66fbe98f273L;
    private static final long k2 = 0x9ae16a3b2f90404fL;
    private static final long k3 = 0xc949d7c7509e6557L;
    private static final long k5 = 0x9ddfea08eb382d69L;


    public static long CityHash64(byte[] s, int index, int len) {
        if (len <= 16 ) {
            return HashLen0to16(s, index, len);
        } else if (len > 16 && len <= 32) {
            return HashLen17to32(s, index, len);
        } else if (len > 32 && len <= 64) {
            return HashLen33to64(s, index, len);
        } else {
            long x = Fetch64(s, index);
            long y = Fetch64(s, index + len - 16) ^ k1;
            long z = Fetch64(s, index + len - 56) ^ k0;
            long[] v = WeakHashLen32WithSeeds(s, len - 64, len, y);
            long[] w = WeakHashLen32WithSeeds(s, len - 32, len * k1, k0);
            z += ShiftMix(v[1]) * k1;
            x = Rotate(z + x, 39) * k1;
            y = Rotate(y, 33) * k1;

            len = (len - 1) & ~63;
            do {
                x = Rotate(x + y + v[0] + Fetch64(s, index + 16), 37) * k1;
                y = Rotate(y + v[1] + Fetch64(s, index + 48), 42) * k1;
                x ^= w[1];
                y ^= v[0];
                z = Rotate(z ^ w[0], 33);
                v = WeakHashLen32WithSeeds(s, index, v[1] * k1, x + w[0]);
                w = WeakHashLen32WithSeeds(s, index + 32, z + w[1], y);
                long t = z;
                z = x;
                x = t;
                index += 64;
                len -= 64;
            } while (len != 0);
            return HashLen16(HashLen16(v[0], w[0]) + ShiftMix(y) * k1 + z,
                    HashLen16(v[1], w[1]) + x);
        }
    }

    private static long HashLen0to16(byte[] s, int index, int len) {
        if (len > 8) {
            long a = Fetch64(s, index);
            long b = Fetch64(s, index + len - 8);
            return HashLen16(a, RotateByAtLeastOne(b + len, len)) ^ b;
        }
        if (len >= 4) {
            long a = Fetch32(s, index);
            return HashLen16(len + (a << 3), Fetch32(s, index + len - 4));
        }
        if (len > 0) {
            byte a = s[index];
            byte b = s[index + len >>> 1];
            byte c = s[index + len - 1];
            int y = (a) + (b << 8);
            int z = len + (c << 2);
            return ShiftMix(y * k2 ^ z * k3) * k2;
        }
        return k2;
    }

    private static long HashLen17to32(byte[] s, int index, int len) {
        long a = Fetch64(s, index) * k1;
        long b = Fetch64(s, index + 8);
        long c = Fetch64(s, index + len - 8) * k2;
        long d = Fetch64(s, index + len - 16) * k0;
        return HashLen16(Rotate(a - b, 43) + Rotate(c, 30) + d,
                a + Rotate(b ^ k3, 20) - c + len);
    }

    private static long HashLen33to64(byte[] s, int index, int len) {
        long z = Fetch64(s, index + 24);
        long a = Fetch64(s, index) + (len + Fetch64(s, index + len - 16)) * k0;
        long b = Rotate(a + z, 52);
        long c = Rotate(a, 37);
        a += Fetch64(s, index + 8);
        c += Rotate(a, 7);
        a += Fetch64(s, index + 16);
        long vf = a + z;
        long vs = b + Rotate(a, 31) + c;
        a = Fetch64(s, index + 16) + Fetch64(s, index + len - 32);
        z = Fetch64(s, index + len - 8);
        b = Rotate(a + z, 52);
        c = Rotate(a, 37);
        a += Fetch64(s, index + len - 24);
        c += Rotate(a, 7);
        a += Fetch64(s, index + len - 16);
        long wf = a + z;
        long ws = b + Rotate(a, 31) + c;
        long r = ShiftMix((vf + ws) * k2 + (wf + vs) * k0);
        return ShiftMix(r * k0 + vs) * k2;
    }

    private static long Fetch64(byte[] p, int index) {
        return toLongLE(p,index);
    }

    private static long Fetch32(byte[] p, int index) {
        return toIntLE(p,index);
    }
    private static long[] WeakHashLen32WithSeeds(
            long w, long x, long y, long z, long a, long b) {
        a += w;
        b = Rotate(b + a + z, 21);
        long c = a;
        a += x;
        a += y;
        b += Rotate(a, 44);
        return new long[]{a + z, b + c};
    }

    private static long[] WeakHashLen32WithSeeds(byte[] s, int index, long a, long b) {
        return WeakHashLen32WithSeeds(Fetch64(s, index),
                Fetch64(s, index + 8),
                Fetch64(s, index + 16),
                Fetch64(s, index + 24),
                a,
                b);
    }

    private static long toLongLE(byte[] b, int i) {
        return 0xffffffffffffffffL & (((long) b[i + 7] << 56) + ((long) (b[i + 6] & 255) << 48) + ((long) (b[i + 5] & 255) << 40) + ((long) (b[i + 4] & 255) << 32) + ((long) (b[i + 3] & 255) << 24) + ((b[i + 2] & 255) << 16) + ((b[i + 1] & 255) << 8) + ((b[i + 0] & 255) << 0));
    }

    private static long toIntLE(byte[] b, int i) {
        return 0xffffffffL & (((b[i + 3] & 255) << 24) + ((b[i + 2] & 255) << 16) + ((b[i + 1] & 255) << 8) + ((b[i + 0] & 255) << 0));
    }

    private static long RotateByAtLeastOne(long val, int shift) {
        return (val >>> shift) | (val << (64 - shift));
    }

    private static long ShiftMix(long val) {
        return val ^ (val >>> 47);
    }

    private static long Uint128Low64(long[] x) {
        return x[0];
    }

    private static long Rotate(long val, int shift) {
        return shift == 0 ? val : (val >>> shift) | (val << (64 - shift));
    }

    private static long Uint128High64(long[] x) {
        return x[1];
    }

    private static long Hash128to64(long[] x) {
        long a = (Uint128Low64(x) ^ Uint128High64(x)) * k5;
        a ^= (a >>> 47);
        long b = (Uint128High64(x) ^ a) * k5;
        b ^= (b >>> 47);
        b *= k5;
        return b;
    }

    private static long HashLen16(long u, long v) {
        return Hash128to64(new long[]{u,v});
    }
    
    public static String CHCityHash64(String[] strs){
        List<String> list = Arrays.asList(strs);
        byte[] bytes = String.join("",list).getBytes();
        return java.lang.Long.toUnsignedString(CityHash64(bytes, 0, bytes.length), 10);
    }
    
    public static void main(String args[]) {
      String[] arr  = new String[1];
        arr[0] = "John";
      System.out.println("Hash = " + CHCityHash64(arr));
    }
}