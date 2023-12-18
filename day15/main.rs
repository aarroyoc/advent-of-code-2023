use std::str::FromStr;

fn main() {
    let contents = std::fs::read_to_string("input").unwrap();
    let msgs = contents.split(',');
    let hash_codes = msgs.map(hash_code);
    let solution1: i64 = hash_codes.sum();
    println!("Solution 1: {}", solution1);

    let mut hashimap = HashiMap::new();
    for msg in contents.split(',') {
	if msg.ends_with("-") {
	    if let Some(tag) = msg.get(0..msg.len() - 1) {
		hashimap.remove(tag);
	    }
	} else {
	    let splits: Vec<&str> = msg.split('=').collect();
	    let tag = splits[0];
	    let focal_length = u8::from_str(splits[1]).unwrap();
	    hashimap.add(tag, focal_length);
	}
    }

    let solution2: i64 = hashimap.focus_power();
    println!("Solution 2: {}", solution2);
}

fn hash_code(code: &str) -> i64 {
    let mut hash = 0;

    for c in code.chars() {
	hash += c as i64;
	hash *= 17;
	hash = hash % 256;
    }
    hash
}

#[derive(Debug)]
struct Lense {
    tag: String,
    focal_length: u8,
}

#[derive(Debug)]
struct HashiMap {
    boxes: Vec<Vec<Lense>>,
}

impl HashiMap {

    fn new() -> Self {
	let mut boxes = Vec::with_capacity(256);

	for _ in 0..256 {
	    boxes.push(Vec::new());
	}
	
	HashiMap {
	    boxes
	}
    }

    fn add(&mut self, tag: &str, focal_length: u8) {
	let hash = hash_code(tag);
	if let Some(vec) = self.boxes.get_mut(hash as usize) {
	    if vec.iter().any(|x| x.tag == tag) {
		for x in vec.iter_mut() {
		    if x.tag == tag {
			x.focal_length = focal_length;
		    }
		}
	    } else {
		vec.push(Lense { tag: tag.to_string(), focal_length });
	    }
	}
    }

    fn remove(&mut self, tag: &str) {
	let hash = hash_code(tag);
	if let Some(vec) = self.boxes.get_mut(hash as usize) {
	    vec.retain(|x| x.tag != tag);
	}
    }

    fn focus_power(&self) -> i64 {
	let mut sum: i64 = 0;
	for i in 0..256 {
	    for (j, lense) in self.boxes[i].iter().enumerate() {
		sum += (i as i64 + 1) * (j as i64 + 1) * lense.focal_length as i64;
	    }
	}
	sum
    }
}
