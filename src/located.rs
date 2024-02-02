use std::ops::Range;

/// Represents a value with a location in source code
#[derive(Debug, PartialEq, Clone)]
pub struct Located<T> {
    pub source_range: Range<usize>,
    pub value: T,
}

impl<T> Located<T> {
    pub fn new(source_range: Range<usize>, value: T) -> Self {
        Located {
            source_range,
            value,
        }
    }

    pub fn with_value<U>(&self, value: U) -> Located<U> {
        Located {
            source_range: self.source_range.clone(),
            value,
        }
    }

    pub fn map<U, E, F>(self, f: F) -> Result<Located<U>, E>
    where
        F: Fn(T) -> Result<U, E>,
    {
        let value = f(self.value)?;
        Ok(Located {
            source_range: self.source_range,
            value,
        })
    }
}
