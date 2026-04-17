#!/usr/bin/env bash
set -euo pipefail

env_file="${1:-}"
target="${2:-}"
secret_file="${3:-}"

if [[ -z "${env_file}" || -z "${target}" ]]; then
  echo "Usage: $0 <env-file> <staging|production> [secret-env-file]" >&2
  exit 1
fi

if [[ "${target}" != "staging" && "${target}" != "production" ]]; then
  echo "Target must be 'staging' or 'production'." >&2
  exit 1
fi

if [[ ! -f "${env_file}" ]]; then
  echo "Env file not found: ${env_file}" >&2
  exit 1
fi

if [[ -n "${secret_file}" && ! -f "${secret_file}" ]]; then
  echo "Secret env file not found: ${secret_file}" >&2
  exit 1
fi

set -a
# shellcheck disable=SC1090
source "${env_file}"
if [[ -n "${secret_file}" ]]; then
  # shellcheck disable=SC1090
  source "${secret_file}"
fi
set +a

required_vars=(
  EASYROC_IMAGE
  EASYROC_IMAGE_TAG
  EASYROC_HOST_PORT
  R_CONFIG_ACTIVE
)

for name in "${required_vars[@]}"; do
  value="${!name:-}"
  if [[ -z "${value}" ]]; then
    echo "Missing required variable: ${name}" >&2
    exit 1
  fi
done

if [[ "${R_CONFIG_ACTIVE}" != "${target}" ]]; then
  echo "R_CONFIG_ACTIVE must match target (${target}), got '${R_CONFIG_ACTIVE}'." >&2
  exit 1
fi

if ! [[ "${EASYROC_HOST_PORT}" =~ ^[0-9]+$ ]]; then
  echo "EASYROC_HOST_PORT must be numeric, got '${EASYROC_HOST_PORT}'." >&2
  exit 1
fi

if [[ "${target}" == "production" && "${EASYROC_IMAGE_TAG}" == "local" ]]; then
  echo "Production deploy cannot use EASYROC_IMAGE_TAG=local." >&2
  exit 1
fi

if [[ -n "${EASYROC_LOG_LEVEL:-}" ]]; then
  level_upper="$(printf "%s" "${EASYROC_LOG_LEVEL}" | tr '[:lower:]' '[:upper:]')"
  case "${level_upper}" in
    DEBUG|INFO|WARN|ERROR) ;;
    *)
      echo "EASYROC_LOG_LEVEL must be one of DEBUG, INFO, WARN, ERROR." >&2
      exit 1
      ;;
  esac
fi

if [[ "${target}" == "production" ]]; then
  if rg -n "^[[:space:]]*[A-Za-z0-9_]*(PASSWORD|TOKEN|SECRET|KEY)[A-Za-z0-9_]*[[:space:]]*=" "${env_file}" >/dev/null 2>&1; then
    echo "Potential secret detected in ${env_file}. Keep production secrets in secret store / secret env file." >&2
    exit 1
  fi
fi

echo "Env validation passed for target '${target}'."
