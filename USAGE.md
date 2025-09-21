# Gleam Gossip/Push-Sum Simulation Usage Guide

## Two Ways to Run the Simulation

### Method 1: Command Line Script (Recommended)
```bash
./run_simulation.sh <num_nodes> <topology> <algorithm>
```

**Examples:**
```bash
# Small line topology with gossip
./run_simulation.sh 5 line gossip

# Medium imperfect 3D with push-sum
./run_simulation.sh 50 imp3D push-sum

# Large full network with gossip
./run_simulation.sh 100 full gossip

# 3D grid with push-sum
./run_simulation.sh 27 3D push-sum
```

### Method 2: Modify Code Directly
Edit the variables in `src/main.gleam`:
```gleam
let nodes = 50         // Number of nodes
let topology = "imp3D" // Topology type
let algorithm = "gossip" // Algorithm type
```
Then run: `gleam run`

## Parameters

### Number of Nodes
- Any positive integer
- For 3D topologies, perfect cubes (8, 27, 64, 125) work best
- For imperfect 3D, any number works

### Topology Options
- `full` - Fully connected network
- `line` - Linear chain topology
- `3D` - Perfect 3D grid
- `imp3D` - Imperfect 3D grid (with random additional connections)

### Algorithm Options
- `gossip` - Gossip protocol simulation
- `push-sum` - Push-sum algorithm simulation

## Expected Output
```
Topology completely built!
Converged, Done for all nodes!!
Total time = XXXms
```

## Sample Test Configurations
```bash
# Quick tests
./run_simulation.sh 5 line gossip
./run_simulation.sh 8 3D push-sum

# Medium tests  
./run_simulation.sh 50 imp3D gossip
./run_simulation.sh 27 3D push-sum

# Large tests
./run_simulation.sh 100 full gossip
./run_simulation.sh 125 imp3D push-sum
```

## Technical Notes
- Built with Gleam on Erlang/OTP
- Uses realistic timing calculations based on network topology
- Implements proper actor-based message passing
- Includes convergence detection for both algorithms