# bc-gitops-demo-counter

Demo counter application for [bc_gitops](https://github.com/beam-campus/bc-gitops) demonstration.

## Overview

This is a simple Erlang gen_server that maintains a counter value. It demonstrates:

1. **Hot Code Reload**: The `code_change/3` callback preserves counter state during upgrades
2. **HTTP API**: Cowboy-based REST endpoints for interaction
3. **GitOps Deployment**: Deployed and upgraded via bc_gitops

## HTTP API

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/count` | Get current counter value |
| POST | `/increment` | Increment counter by 1 |
| POST | `/reset` | Reset counter to 0 |
| GET | `/health` | Health check endpoint |

## Quick Start

```bash
# Clone and build
git clone https://github.com/beam-campus/bc-gitops-demo-counter.git
cd bc-gitops-demo-counter
rebar3 compile

# Run in shell
rebar3 shell

# Test the API
curl http://localhost:8080/count
curl -X POST http://localhost:8080/increment
curl http://localhost:8080/health
```

## Hot Code Reload Demo

The key feature this app demonstrates is state preservation during hot code reload:

1. Start the app and increment the counter several times
2. Modify the code (e.g., change a log message)
3. Use bc_gitops to upgrade the app
4. The counter value is preserved!

This is possible because of the `code_change/3` callback in `demo_counter_server.erl`.

## GitOps Deployment

To deploy via bc_gitops, add an `app.config` to your GitOps repo:

```erlang
#{
    name => demo_counter,
    source => {hex, "demo_counter", "0.1.0"},
    config => #{
        http_port => 8081
    }
}.
```

## Configuration

| Key | Default | Description |
|-----|---------|-------------|
| `http_port` | 8080 | Port for HTTP API |

## Related

- [bc_gitops](https://github.com/beam-campus/bc-gitops) - GitOps for the BEAM
- [bc-gitops-demo-web](https://github.com/beam-campus/bc-gitops-demo-web) - Host application
- [bc-gitops-demo-repo](https://github.com/beam-campus/bc-gitops-demo-repo) - GitOps specs

## License

MIT
