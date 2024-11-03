use std::{collections::HashMap, fmt::Display};

#[allow(dead_code)]
pub fn hashmap_to_string<K, V>(hashmap: &HashMap<K, V>) -> String
where
    K: Display,
    V: Display,
{
    hashmap
        .iter()
        .map(|(k, v)| format!("[{} - {}]", k, v))
        .collect()
}
