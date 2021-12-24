use std::fmt::{Debug, Display, Formatter};
use std::ops::RangeInclusive;

#[derive(Eq, PartialEq)]
pub struct Struct<'a> {
    pub name: &'a str,
    pub fields: Vec<Entry<'a>>,
}

impl<'a> Debug for Struct<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut s = f.debug_struct(self.name);

        for elem in &self.fields {
            s.field(elem.key, &elem.value);
        }

        s.finish()
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Entry<'a> {
    key: &'a str,
    value: Value<'a>,
}

#[derive(Eq, PartialEq)]
pub enum Value<'a> {
    FieldValue(&'a str),
    Struct(Struct<'a>),
    Array(Vec<Value<'a>>),
}

impl<'a> Debug for Value<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::FieldValue(field) => f.write_fmt(format_args!("{:?}", field)),
            Value::Struct(s) => f.write_fmt(format_args!("{:?}", s)),
            Value::Array(arr) => f.write_fmt(format_args!("{:?}", arr)),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
#[repr(u8)]
pub enum QueryError {
    Invalid,
    NotFound,
}

impl Display for QueryError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let output = match self {
            QueryError::Invalid => "invalid",
            QueryError::NotFound => "not found",
        };
        f.write_str(output)
    }
}

impl<'a> TryFrom<&'a str> for Value<'a> {
    type Error = ();

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        let (v, _) = get_value(value).ok_or(())?;
        Ok(v)
    }
}

impl<'a> Value<'a> {
    pub fn query_value(&self, mut query: &str) -> Result<&Value, QueryError> {
        let first = query.chars().next();
        // must start with `.`
        if first != Some('.') {
            return Err(QueryError::Invalid);
        }

        query = &query[1..];

        let mut on = self;

        loop {
            if query.is_empty() {
                return Ok(on);
            }

            let next_dot = query.find('.');

            let key = match next_dot {
                Some(next_dot) => &query[..next_dot],
                None => query,
            };

            let next = match on {
                Value::FieldValue(..) => return Err(QueryError::Invalid), // we are trying to query a field
                Value::Struct(s) => {
                    let entry = s
                        .fields
                        .iter()
                        .find(|entry| entry.key == key)
                        .ok_or(QueryError::NotFound)?;
                    &entry.value
                }
                Value::Array(a) => {
                    let idx: usize = key.parse().map_err(|_| QueryError::Invalid)?;
                    &a[idx]
                }
            };

            on = next;
            match next_dot {
                None => {
                    return Ok(next);
                }
                Some(dot) => query = &query[dot + 1..],
            }
        }
    }
}

fn opt_min<T: Ord>(a: Option<T>, b: Option<T>) -> Option<T> {
    match (a, b) {
        (Some(a), None) => Some(a),
        (None, Some(b)) => Some(b),
        (Some(a), Some(b)) => Some(a.min(b)),
        _ => None,
    }
}

fn get_values(input: &str) -> Vec<Value> {
    input
        .split(',')
        .map(|elem| get_value(elem).unwrap().0)
        .collect()
}

/// Returns the value end the end idx of it
fn get_value(input: &str) -> Option<(Value, usize)> {
    if input.trim().is_empty() {
        return None;
    }

    let first_comma = input.find(',');

    let first_bracket = input.find('[');
    let first_parenthesis = input.find('(');
    let first_brace = input.find('{');

    if let Some(min_arr_like) = opt_min(first_bracket, first_parenthesis) {
        let do_this = match opt_min(first_comma, first_brace) {
            Some(other) if min_arr_like < other => true,
            None => true,
            _ => false,
        };
        if do_this {
            let span = match (first_bracket, first_parenthesis) {
                (Some(a), Some(b)) if a < b => span::<'[', ']'>(&input[a..]),
                (Some(a), Some(b)) if a > b => span::<'(', ')'>(&input[b..]),
                (Some(a), _) => span::<'[', ']'>(&input[a..]),
                (_, Some(b)) => span::<'(', ')'>(&input[b..]),
                _ => None,
            };

            if let Some(range) = span {
                let end = *range.end();

                let v = Value::Array(get_values(&input[range]));

                return Some((v, end));
            }
        }
    }

    let res = match (first_comma, first_brace) {
        (Some(comma), None) => (Value::FieldValue(&input[..comma]), comma),
        (Some(comma), Some(brace)) if comma < brace => (Value::FieldValue(&input[..comma]), comma),
        _ => match span::<'{', '}'>(input) {
            None => (Value::FieldValue(input.trim()), input.len()),
            Some(range) => {
                let type_name = input[..(*range.start() - 1)].trim();

                let end = *range.end();

                let v = Value::Struct(Struct {
                    name: type_name,
                    fields: entries(&input[range]),
                });

                (v, end)
            }
        },
    };

    Some(res)
}

fn entries(mut input: &str) -> Vec<Entry> {
    let mut entries = Vec::new();
    loop {
        let colon_idx = match input.find(':') {
            None => return entries,
            Some(idx) => idx,
        };

        let key = input[..colon_idx].trim();
        let value_str = &input[(colon_idx + 1)..];
        match get_value(value_str) {
            None => return entries,
            Some((value, idx)) => {
                let entry = Entry { key, value };
                entries.push(entry);
                input = &input[(colon_idx + idx + 1)..];
            }
        }
    }
}

fn span<const OPEN: char, const CLOSE: char>(input: &str) -> Option<RangeInclusive<usize>> {
    let mut left_count = 0;

    let mut start = None;

    for (idx, c) in input.chars().enumerate() {
        if c == OPEN {
            if start == None {
                start = Some(idx + 1);
            }
            left_count += 1;
        } else if c == CLOSE {
            if left_count == 0 {
                return None;
            }
            left_count -= 1;
            if left_count == 0 {
                return Some(start.unwrap()..=(idx - 1));
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use crate::{get_value, Entry, Struct, Value};

    macro_rules! get_value {
        ($var: ident = $value: expr) => {
            let formatted = &format!("{:?}", $value);
            let ($var, _) = get_value(&formatted).unwrap();
        };
    }

    #[test]
    fn test_arr_query() {
        get_value!(input = [1123]);
        let should_be = Value::Array(vec![Value::FieldValue("1123")]);

        assert_eq!(input, should_be);

        get_value!(elem_0 = 1123);

        println!("input {:?}", input);
        assert_eq!(input.query_value(".").unwrap(), &input);
        assert_eq!(input.query_value(".0").unwrap(), &elem_0);
    }

    #[test]
    fn test_debug_arr() {
        let arr = [123213, 123, 123, 123];

        let arr_strs = arr.map(|x| format!("{}", x));
        let elems: Vec<_> = arr_strs
            .iter()
            .map(|elem| Value::FieldValue(elem))
            .collect();

        get_value!(v = arr);
        let should_be = Value::Array(elems);

        assert_eq!(v, should_be);
    }

    #[test]
    fn test_debug_tuples() {
        let formatted = format!("{:?}", (123213, 123, 123, 123));

        let vec_strs = vec!["123213", "123", "123", "123"];
        let elems: Vec<_> = vec_strs
            .iter()
            .map(|elem| Value::FieldValue(elem))
            .collect();

        let (v, _) = get_value(&formatted).unwrap();
        let should_be = Value::Array(elems);

        assert_eq!(v, should_be);
    }

    #[test]
    fn test_debug_empty() {
        #[derive(Debug)]
        struct Empty;

        #[derive(Debug)]
        #[allow(dead_code)]
        struct HasEmpty {
            empty: Empty,
        }

        let res = format!("{:?}", HasEmpty { empty: Empty });

        let should_be = Value::Struct(Struct {
            name: "HasEmpty",
            fields: vec![Entry {
                key: "empty",
                value: Value::FieldValue("Empty"),
            }],
        });

        let (v, _) = get_value(&res).unwrap();
        assert_eq!(v, should_be);

        println!("{:?}", v);
    }
}
