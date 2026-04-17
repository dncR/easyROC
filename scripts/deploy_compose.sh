#!/usr/bin/env bash
set -euo pipefail

target="${1:-staging}"
env_file="${2:-.env}"

if [[ "${target}" != "staging" && "${target}" != "production" ]]; then
  echo "Usage: $0 [staging|production] [env-file]" >&2
  exit 1
fi

if [[ ! -f "${env_file}" ]]; then
  echo "Env file not found: ${env_file}" >&2
  exit 1
fi

compose_args=(
  --env-file "${env_file}"
  -f docker-compose.yml
  -f "docker-compose.${target}.yml"
)

if [[ "${target}" == "staging" ]]; then
  docker compose "${compose_args[@]}" up -d --build --remove-orphans
else
  docker compose "${compose_args[@]}" up -d --remove-orphans
fi

docker compose "${compose_args[@]}" ps

