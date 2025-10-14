#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use banquo::operators::{Always, And, Eventually, Implies};
    use banquo::predicate::Term;
    use banquo::{EvaluationError, Formula, Predicate, Trace};
    use banquo::{evaluate, predicate};
    use telo::{monitor::*, predicate::*, *};

    #[test]
    fn ltl() {
        // Step 1: define predicates
        const LIMIT: i32 = 123;

        let mut builder = Predicates::builder();
        let above_limit = builder.new_predicate(ClosurePredicate::new(
            |val: &i32| *val >= LIMIT,
            "value is above LIMIT",
        ));
        let predicates = builder.build();

        // Step 2: define temporal specification
        let property: Property = Property::never(Property::atomic(above_limit));

        // Step 3: transform to monitoring automaton
        let automaton = property.to_monitoring_automaton(&predicates);

        // Step 4: runtime monitoring
        let mut monitor = Monitor::new(predicates, automaton);
        for value in 0..LIMIT {
            assert!(monitor.next_state(&value));
        }
        assert!(monitor.next_state(&LIMIT)); // the property is violated       
    }

    fn get_eq(var_name: &str, val: f64) -> And<Predicate, Predicate> {
        let p1 = Predicate::from([Term::from(var_name), Term::from(val)]);
        let p2 = Predicate::from([Term::from((var_name, -1.0)), Term::from(-val)]);
        And::new(p1, p2)
    }

    #[test]
    fn stl() {
        fn state(v1: f64, v2: f64) -> HashMap<&'static str, f64> {
            HashMap::from([("gear", v1), ("rpm", v2)])
        }

        // [] ((gear = 3 /\ rpm >= 4000) -> <>[0,3] gear = 4)
        let trace = Trace::from([
            (0, state(1.0, 5.0)),
            (1, state(4.0, 5.0)),
            (2, state(4.0, 5.0)),
            (3, state(4.0, 5.0)),
            (4, state(4.0, 5.0)),
            // (2.0, state(1.0, 4030.0)),
            // (3.0, state(1.0, 4030.0)),
            // (4.0, state(1.0, 4030.0)),
        ]);

        let gear_is_3 = get_eq("gear", 3.0);

        let rpm_gte_4000 = predicate! {-1.0*rpm <=-4.0};
        let left = And::new(gear_is_3, rpm_gte_4000);
        let right = Eventually::bounded(0..=3, get_eq("gear", 4.0));
        let implies = Implies::new(left, right);

        let final_ = Always::unbounded(implies);
        let result: Result<f64, EvaluationError> = evaluate(&trace, &final_);
        println!("{:?}", result);
        result.unwrap();
    }
}
