# Gleam Gossip & Push-Sum Simulator (Gleam 1.12.0)

This repository contains a simulator for the **Gossip** and **Push-Sum** algorithms, implemented in [Gleam](https://gleam.run/) 1.12.0.  
The project models message-passing actors using Gleam’s lightweight concurrency features and supports different network topologies.

---

## Team Members

- **Avighna Yarlagadda**  
  - UFID: 40987768

- **Manikanta Srinivas Penumarthi**  
  - UFID: 95550186

## Build & Run

### Prerequisites
- [Erlang/OTP](https://www.erlang.org/)
- [Gleam 1.12.0](https://gleam.run/)
- Optionally `rebar3` if you want to compile using Erlang tooling

### Build
From the project root:

```bash
gleam build
```

### Run
Use `gleam run -- <numNodes> <topology> <algorithm>` where:
- `numNodes` → number of nodes in the network (e.g., 100, 500, 1000)
- `topology` → one of `full`, `line`, `3D`, or `imp3D`
- `algorithm` → `gossip` or `push-sum`

#### Example Runs
```powershell
PS> gleam run -- 100 full gossip
   Compiled in 0.05s
    Running project2_gleam_simulator.main
Running project2...
Nodes: 100
Topology: full
Algorithm: gossip

PS> gleam run -- 500 line push-sum
   Compiled in 0.05s
    Running project2_gleam_simulator.main
Running project2...
Nodes: 500
Topology: line
Algorithm: push-sum

PS> gleam run -- 1000 imp3D gossip
   Compiled in 0.05s
    Running project2_gleam_simulator.main
Running project2...
Nodes: 1000
Topology: imp3D
Algorithm: gossip
```

---

## File Structure
- `gleam.toml` – project configuration
- `src/project2_gleam_simulator.gleam` – main entry point
- `src/topology.gleam` – topology definitions
- `src/node.gleam` – actor/node logic
- `src/gossip.gleam` – Gossip protocol implementation
- `src/push_sum.gleam` – Push-Sum protocol implementation
- `src/coordinator.gleam` – convergence coordination
- `run.sh` – helper script (optional)

---

## Notes
- The simulator prints node count, topology, and algorithm when invoked.  
- Convergence time measurement and algorithm logic should be implemented in the respective protocol modules.  
- Tested with Gleam 1.12.0 on Windows + Erlang/OTP.  

---