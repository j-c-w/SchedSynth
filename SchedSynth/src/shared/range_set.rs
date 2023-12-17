pub struct IntegerRangeSet {
    ranges: Vec<RangeType<i32>>
}

pub struct AnyIntegerSet {

}

pub enum RangeType<Item> {
    Between(Item, Item),
    Set(Vec<Item>)
}

pub trait Range<Item> {
    fn contains(&self, i: Item) -> bool;
    fn to_string(&self) -> String;
}

impl ToString for RangeType<i32> {
    fn to_string(&self) -> String {
        match self {
            RangeType::Between(f, t) => f.to_string() + " -- " + t.to_string(),
            RangeType::Set(items) => "{" + items.to_string() + "}"
        }
    }
}

impl Range<i32> for IntegerRangeSet {
    fn contains(&self, i: i32) -> bool {
        for range in self.ranges {
            match range {
                RangeType::Between(from, to) => {
                    if (i >= from) && (i <= to) {
                        return true;
                    }
                },
                RangeType::Set(items) => {
                    for item in items {
                        if (i == item) {
                            return true;
                        }
                    }
                }
            }
        }

        return false;
    }

    fn to_string(&self) -> String {
        let mut result = "<";

        for range in self.ranges {
            result.push(range.to_string())
        }
        result.push(">");

        return result;
    }
}

impl Range<Item> for AnyIntegerSet {
    fn contains(&self, i: Item) -> bool {
        return true;
    }

    fn to_string(&self) -> String {
        "??"
    }
}


