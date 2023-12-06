pub fn ways_to_win(distance_to_beat: usize, total_race_time: usize) -> usize {
    let min_charging_time = get_min_charging_time(distance_to_beat, total_race_time)
        .expect("min charging time should exist");
    let max_charging_time = get_max_charging_time(distance_to_beat, total_race_time)
        .expect("max charging time should exist");

    assert!(max_charging_time.ge(&min_charging_time));
    max_charging_time - min_charging_time + 1
}

fn get_min_charging_time(distance_to_beat: usize, total_race_time: usize) -> Option<usize> {
    for i in 1..total_race_time {
        let distance = total_distance(i, total_race_time);

        if distance > distance_to_beat {
            return Some(i);
        }
    }

    None
}

fn get_max_charging_time(time_to_beat: usize, total_race_time: usize) -> Option<usize> {
    for i in (1..total_race_time).rev() {
        let distance = total_distance(i, total_race_time);

        if distance > time_to_beat {
            return Some(i);
        }
    }

    None
}

fn total_distance(charging_time: usize, total_race_time: usize) -> usize {
    let remaining_time = total_race_time - charging_time;

    remaining_time * charging_time // charging_time = speed
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case((7, 9), 4)]
    #[case((15, 40), 8)]
    #[case((30, 200), 9)]
    fn calculates_ways_to_win_correctly(#[case] input: (usize, usize), #[case] expected: usize) {
        assert_eq!(ways_to_win(input.1, input.0), expected);
    }
}
