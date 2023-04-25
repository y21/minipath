use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::{char, digit1, space0};
use nom::character::streaming::space1;
use nom::combinator::{map, map_res, opt};
use nom::multi::{separated_list0, separated_list1};
use nom::sequence::{delimited, tuple};
use nom::IResult;

#[derive(Debug)]
pub enum Mutability {
    /// Immutable
    Immut,
    /// Mutable
    Mut,
}

#[derive(Debug)]
pub enum Ty<'a> {
    /// &T, &mut T
    Ref(Mutability, Box<Ty<'a>>),
    /// *const T, *mut T
    RawPtr(Mutability, Box<Ty<'a>>),
    /// fn(T, U) -> V
    FnPtr {
        params: Vec<Ty<'a>>,
        ret: Option<Box<Ty<'a>>>,
    },
    /// !
    Never,
    /// dyn Trait
    Dyn(Box<Path<'a>>),
    /// (T, U, P, L, E)
    Tuple(Vec<Ty<'a>>),
    /// foo, foo::bar::baz
    Path(Box<Path<'a>>),
    /// [T; N]
    Array(Box<Ty<'a>>, usize),
    /// [T]
    Slice(Box<Ty<'a>>),
    /// _
    Infer,
}

#[derive(Debug)]
pub struct PathSegment<'a> {
    pub ident: &'a str,
}

/// Qualified self, e.g. `<Type as Trait>::`
#[derive(Debug)]
pub struct QSelf<'a> {
    pub ty: Ty<'a>,
    pub target: Box<Path<'a>>,
}

/// Path, including qualified self if present
#[derive(Debug)]
pub struct Path<'a> {
    pub qself: Option<QSelf<'a>>,
    pub segments: Vec<PathSegment<'a>>,
}

fn parse_ident(input: &str) -> IResult<&str, &str> {
    take_while1(char::is_alphanumeric)(input)
}

fn parse_num(input: &str) -> IResult<&str, usize> {
    map_res(digit1, |s: &str| s.parse())(input)
}

fn parse_mutability(input: &str) -> IResult<&str, Mutability> {
    map(opt(tag("mut")), |m| match m {
        Some(_) => Mutability::Mut,
        None => Mutability::Immut,
    })(input)
}

fn parse_raw_mutability(input: &str) -> IResult<&str, Mutability> {
    alt((
        map(tag("const"), |_| Mutability::Immut),
        map(tag("mut"), |_| Mutability::Mut),
    ))(input)
}

fn parse_fn(input: &str) -> IResult<&str, (Vec<Ty<'_>>, Option<Box<Ty<'_>>>)> {
    let (input, _) = tag("fn")(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char('(')(input)?;
    let (input, params) = separated_list0(tuple((char(','), space0)), parse_ty)(input)?;
    let (input, _) = char(')')(input)?;
    let (input, _) = space0(input)?;
    let (input, ret) = opt(map(tuple((tag("->"), space0, parse_ty)), |(.., ty)| ty))(input)?;

    Ok((input, (params, ret.map(Box::new))))
}

pub fn parse_ty(input: &str) -> IResult<&str, Ty> {
    alt((
        map(char('_'), |_| Ty::Infer),
        map(char('!'), |_| Ty::Never),
        map(tuple((tag("dyn"), space1, parse_path)), |(.., p)| {
            Ty::Dyn(Box::new(p))
        }),
        map(
            delimited(
                char('('),
                separated_list1(tuple((char(','), space1)), parse_ty),
                char(')'),
            ),
            Ty::Tuple,
        ),
        map(
            tuple((char('&'), parse_mutability, space0, parse_ty)),
            |(_, m, _, ty)| Ty::Ref(m, Box::new(ty)),
        ),
        map(
            tuple((char('*'), parse_raw_mutability, space0, parse_ty)),
            |(_, m, _, ty)| Ty::RawPtr(m, Box::new(ty)),
        ),
        map(delimited(char('['), parse_ty, char(']')), |v| {
            Ty::Slice(Box::new(v))
        }),
        map(
            delimited(
                char('['),
                tuple((parse_ty, char(';'), space1, parse_num)),
                char(']'),
            ),
            |(ty, .., size)| Ty::Array(Box::new(ty), size),
        ),
        map(parse_fn, |(params, ret)| Ty::FnPtr { params, ret }),
        map(parse_path, |p| Ty::Path(Box::new(p))),
    ))(input)
}

pub fn parse_qself(input: &str) -> IResult<&str, QSelf> {
    let (input, _) = char('<')(input)?;
    let (input, ty) = parse_ty(input)?;
    let (input, _) = space1(input)?;
    let (input, _) = tag("as")(input)?;
    let (input, _) = space1(input)?;
    let (input, target) = parse_path(input)?;
    let (input, _) = char('>')(input)?;
    let (input, _) = tag("::")(input)?;

    Ok((
        input,
        QSelf {
            target: Box::new(target),
            ty,
        },
    ))
}

pub fn parse_path(input: &str) -> IResult<&str, Path> {
    let (input, qself) = opt(parse_qself)(input)?;
    let (input, segments) =
        separated_list1(tag("::"), map(parse_ident, |ident| PathSegment { ident }))(input)?;
    Ok((input, Path { qself, segments }))
}
