# Examples Directory

This directory contains examples and quick references for using the Prolog Transit Optimizer.

---

## 📁 Files

### 1. [quick-reference.md](quick-reference.md) ⭐ START HERE
**What it is:** Command-line quick reference guide
**Use it for:** Copy-paste terminal commands and one-liners

**Contains:**
- Preparation steps (how to start SWI-Prolog, load data)
- Terminal commands you can copy and paste
- One-liner commands for quick tasks
- Common search patterns
- Troubleshooting tips

**Example usage:**
```bash
# Copy a command from quick-reference.md and paste in terminal
swipl --stack_limit=4g -g "
    consult('src/query/api.pl'),
    consult('src/query/search_helpers.pl'),
    load_gtfs('data/gtfs/toronto-ttc'),
    list_stops_containing('station'),
    halt.
"
```

**Best for:**
- Quick searches
- Finding stops/routes by name
- Learning common commands
- When you want to run something without entering Prolog REPL

---

### 2. [example_queries.pl](example_queries.pl)
**What it is:** Prolog module with demo predicates
**Use it for:** Interactive demos and code examples

**Contains:**
- Prolog predicates you can call
- Demo functions showing different features
- Code examples you can study
- Pretty-printing functions

**Example usage:**
```prolog
% Start Prolog
swipl

% Load the system and data
?- consult('src/query/api.pl').
?- load_gtfs('data/gtfs/mmt').

% Load the examples module
?- consult('examples/example_queries.pl').

% Run a demo
?- demo_basic_routing.
?- demo_accessibility.

% Or run all demos
?- run_all_demos.
```

**Best for:**
- Learning how to write Prolog queries
- Understanding the API
- Interactive exploration
- Code examples to study and modify

---

## 🎯 Which File Should I Use?

### Use **quick-reference.md** when:
- ✅ You want to quickly search for stops/routes
- ✅ You want copy-paste terminal commands
- ✅ You're learning the system
- ✅ You want one-line commands
- ✅ You need troubleshooting help

### Use **example_queries.pl** when:
- ✅ You're writing Prolog code
- ✅ You want to see how to use the API
- ✅ You want interactive demos
- ✅ You're learning Prolog query patterns
- ✅ You want to study working code examples

---

## 🚀 Quick Start

### Option A: Terminal Commands (quick-reference.md)

1. Open [quick-reference.md](quick-reference.md)
2. Find the command you need
3. Copy and paste into your terminal

**Example:**
```bash
# Find subway stations in Toronto
swipl --stack_limit=4g -g "
    consult('src/query/api.pl'),
    consult('src/query/search_helpers.pl'),
    load_gtfs('data/gtfs/toronto-ttc'),
    list_stops_containing('station'),
    halt.
"
```

### Option B: Interactive Prolog (example_queries.pl)

1. Start Prolog: `swipl`
2. Load system: `?- consult('src/query/api.pl').`
3. Load data: `?- load_gtfs('data/gtfs/mmt').`
4. Load examples: `?- consult('examples/example_queries.pl').`
5. Run demos: `?- demo_basic_routing.`

---

## 📚 Learning Path

### Beginner (Day 1)
1. Read **quick-reference.md** → Preparation Steps
2. Try the one-liner commands
3. Search for stops in your area

### Intermediate (Week 1)
1. Read **example_queries.pl** code
2. Load it in Prolog and run demos
3. Modify example queries for your use case

### Advanced (Week 2+)
1. Write your own predicates
2. Combine examples in new ways
3. Contribute new examples back!

---

## 💡 Common Tasks

### Task 1: Find a Stop by Name
**File:** [quick-reference.md](quick-reference.md) → Search Commands
```bash
swipl --stack_limit=4g -g "
    consult('src/query/api.pl'),
    consult('src/query/search_helpers.pl'),
    load_gtfs('data/gtfs/toronto-ttc'),
    list_stops_containing('union'),
    halt.
"
```

### Task 2: Find a Route Between Two Stops
**File:** [example_queries.pl](example_queries.pl) → demo_basic_routing
```prolog
?- consult('examples/example_queries.pl').
?- demo_basic_routing.
```

### Task 3: Check Wheelchair Accessibility
**File:** [example_queries.pl](example_queries.pl) → demo_accessibility
```prolog
?- demo_accessibility.
```

---

## 📖 Related Documentation

- **[../README.md](../README.md)** - Main project documentation
- **[../TESTING-GUIDE.md](../TESTING-GUIDE.md)** - How to test the system
- **[../USING-SEARCH-HELPERS.md](../USING-SEARCH-HELPERS.md)** - Detailed search examples
- **[../session_notes/](../session_notes/)** - Development notes and implementation details

---

## 🔗 External Resources

### GTFS Data
- [Toronto TTC](https://open.toronto.ca/dataset/ttc-routes-and-schedules/)
- [Portland TriMet](https://developer.trimet.org/GTFS.shtml)
- [GTFS Reference](https://gtfs.org/schedule/reference/)

### Prolog Learning
- [Learn Prolog Now](http://www.learnprolognow.org/)
- [SWI-Prolog Manual](https://www.swi-prolog.org/pldoc/doc_for?object=manual)

---

## 🤝 Contributing Examples

Have a useful query or command? Add it!

### Adding a Terminal Command
1. Edit [quick-reference.md](quick-reference.md)
2. Add it to the appropriate section
3. Include a comment explaining what it does

### Adding a Prolog Example
1. Edit [example_queries.pl](example_queries.pl)
2. Add a new `demo_*` predicate
3. Add it to the module exports
4. Document with comments

---

**Quick Links:**
- 🚀 [Quick Reference](quick-reference.md) - Terminal commands
- 📝 [Example Queries](example_queries.pl) - Prolog code
- 📖 [Main README](../README.md) - Project overview

**Last Updated:** November 2, 2025
