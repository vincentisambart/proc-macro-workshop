// Crates that have the "proc-macro" crate type are only allowed to export
// procedural macros. So we cannot have one crate that defines procedural macros
// alongside other types of public APIs like traits and structs.
//
// For this project we are going to need a #[bitfield] macro but also a trait
// and some structs. We solve this by defining the trait and structs in this
// crate, defining the attribute macro in a separate bitfield-impl crate, and
// then re-exporting the macro from this crate so that users only have one crate
// that they need to import.
//
// From the perspective of a user of this crate, they get all the necessary APIs
// (macro, trait, struct) through the one bitfield crate.
use bitfield_impl::declare_types;
pub use bitfield_impl::{bitfield, BitfieldSpecifier};

pub trait Specifier
where
    Self: Sized,
{
    const BITS: usize;
    type OutsideRepr;

    fn try_from(value: u64) -> Option<Self::OutsideRepr>;
}

impl Specifier for bool {
    const BITS: usize = 1;
    type OutsideRepr = Self;

    fn try_from(value: u64) -> Option<Self::OutsideRepr> {
        match value {
            0 => Some(false),
            1 => Some(true),
            _ => None,
        }
    }
}

pub struct Holder<T>(T);

pub trait BestFit {
    type Unsigned;
}

declare_types!();

pub mod checks {
    pub enum ZeroMod8 {}
    pub enum OneMod8 {}
    pub enum TwoMod8 {}
    pub enum ThreeMod8 {}
    pub enum FourMod8 {}
    pub enum FiveMod8 {}
    pub enum SixMod8 {}
    pub enum SevenMod8 {}

    pub trait Mod8Name {
        type Name;
    }

    impl Mod8Name for crate::Holder<[(); 7]> {
        type Name = SevenMod8;
    }
    impl Mod8Name for crate::Holder<[(); 6]> {
        type Name = SixMod8;
    }
    impl Mod8Name for crate::Holder<[(); 5]> {
        type Name = FiveMod8;
    }
    impl Mod8Name for crate::Holder<[(); 4]> {
        type Name = FourMod8;
    }
    impl Mod8Name for crate::Holder<[(); 3]> {
        type Name = ThreeMod8;
    }
    impl Mod8Name for crate::Holder<[(); 2]> {
        type Name = TwoMod8;
    }
    impl Mod8Name for crate::Holder<[(); 1]> {
        type Name = OneMod8;
    }
    impl Mod8Name for crate::Holder<[(); 0]> {
        type Name = ZeroMod8;
    }

    pub trait TotalSizeIsMultipleOfEightBits {}

    impl TotalSizeIsMultipleOfEightBits for ZeroMod8 {}

    pub struct MultipleOfEightBitsSizeRequired<T: TotalSizeIsMultipleOfEightBits>(T);

    impl<T: TotalSizeIsMultipleOfEightBits> MultipleOfEightBitsSizeRequired<T> {}

    pub trait DiscriminantInRange {}
    pub enum True {}
    pub enum False {}
    impl DiscriminantInRange for True {}

    pub trait Bool {
        type Value;
    }

    impl Bool for crate::Holder<[(); 0]> {
        type Value = False;
    }

    impl Bool for crate::Holder<[(); 1]> {
        type Value = True;
    }

    pub struct RequireDiscriminantInRange<T: DiscriminantInRange>(T);
}
