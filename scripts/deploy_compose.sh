#!/usr/bin/env bash
set -euo pipefail

target="${1:-staging}"
env_file="${2:-.env}"
secret_file="${3:-}"

if [[ "${target}" != "staging" && "${target}" != "production" ]]; then
  echo "Usage: $0 [staging|production] [env-file] [secret-env-file]" >&2
  exit 1
fi

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
"${script_dir}/validate_env.sh" "${env_file}" "${target}" "${secret_file}"

compose_args=(
  --env-file "${env_file}"
  -f docker-compose.yml
  -f "docker-compose.${target}.yml"
)

if [[ -n "${secret_file}" ]]; then
  compose_args+=(--env-file "${secret_file}")
fi

if [[ "${target}" == "staging" ]]; then
  docker compose "${compose_args[@]}" up -d --build --remove-orphans
else
  docker compose "${compose_args[@]}" up -d --remove-orphans
fi

docker compose "${compose_args[@]}" ps
