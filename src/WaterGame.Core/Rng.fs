namespace WaterGame.Core

/// SplitMix64。実行環境に依存せず、同じシードから同じ列を再現するために自前で持つ。
type Rng = { State: uint64 }

module Rng =
    let create (seed: int) =
        { State = uint64 (uint32 seed) * 0x9E3779B97F4A7C15UL + 0x2545F4914F6CDD1DUL }

    let nextU64 (r: Rng) =
        let s = r.State + 0x9E3779B97F4A7C15UL
        let z = (s ^^^ (s >>> 30)) * 0xBF58476D1CE4E5B9UL
        let z = (z ^^^ (z >>> 27)) * 0x94D049BB133111EBUL
        (z ^^^ (z >>> 31)), { State = s }

    /// [0, 1) の一様乱数
    let nextFloat (r: Rng) =
        let v, r' = nextU64 r
        float (v >>> 11) / 9007199254740992.0, r'

    let nextRange (lo: float) (hi: float) (r: Rng) =
        let u, r' = nextFloat r
        lo + (hi - lo) * u, r'
