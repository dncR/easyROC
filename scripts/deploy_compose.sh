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

wait_seconds="${EASYROC_HEALTHCHECK_WAIT_SECONDS:-120}"
if ! [[ "${wait_seconds}" =~ ^[0-9]+$ ]]; then
  echo "EASYROC_HEALTHCHECK_WAIT_SECONDS must be numeric." >&2
  exit 1
fi

container_id="$(docker compose "${compose_args[@]}" ps -q easyroc)"
if [[ -z "${container_id}" ]]; then
  echo "Could not resolve container id for service 'easyroc'." >&2
  exit 1
fi

start_ts="$(date +%s)"
while true; do
  health_status="$(docker inspect --format '{{if .State.Health}}{{.State.Health.Status}}{{else}}none{{end}}' "${container_id}")"

  if [[ "${health_status}" == "healthy" ]]; then
    echo "Service 'easyroc' is healthy."
    break
  fi

  if [[ "${health_status}" == "unhealthy" ]]; then
    echo "Service 'easyroc' became unhealthy." >&2
    docker compose "${compose_args[@]}" logs --tail 80 easyroc >&2 || true
    exit 1
  fi

  now_ts="$(date +%s)"
  elapsed=$((now_ts - start_ts))
  if (( elapsed >= wait_seconds )); then
    echo "Timed out waiting for healthy state after ${wait_seconds}s (status=${health_status})." >&2
    docker compose "${compose_args[@]}" logs --tail 80 easyroc >&2 || true
    exit 1
  fi

  sleep 2
done

docker compose "${compose_args[@]}" ps
