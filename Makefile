.PHONY: verify-doctor verify-plan verify-smoke verify-focused verify-compat \
	verify-harness verify-release-core verify-downstream verify-reverse \
	verify-documentation verify-release-compat verify-self-test

# The Python controller, not Make's homogeneous jobserver, owns weighted
# CPU/RAM/scratch scheduling.  VERIFY_ARGS is the intentional escape hatch for
# --worker-image, --since, --param, --containment, and the explicit
# development-only --best-effort mode.  Once the reviewed root-owned helper is
# installed, scripts/verify enters aggregate systemd containment by default on
# this machine.

verify-doctor:
	./scripts/verify doctor $(VERIFY_ARGS)

verify-plan:
	./scripts/verify plan --profile focused $(VERIFY_ARGS)

verify-smoke:
	./scripts/verify run --profile smoke $(VERIFY_ARGS)

verify-focused:
	./scripts/verify run --profile focused $(VERIFY_ARGS)

verify-compat:
	./scripts/verify run --profile compat $(VERIFY_ARGS)

verify-harness:
	./scripts/verify run --profile harness $(VERIFY_ARGS)

verify-release-core:
	./scripts/verify run --profile release-core $(VERIFY_ARGS)

verify-downstream:
	./scripts/verify run --profile prepared-downstream $(VERIFY_ARGS)

verify-reverse:
	./scripts/verify run --profile prepared-reverse $(VERIFY_ARGS)

verify-documentation:
	./scripts/verify run --profile prepared-documentation $(VERIFY_ARGS)

verify-release-compat:
	./scripts/verify run --profile prepared-release-compat $(VERIFY_ARGS)

verify-self-test:
	./scripts/verify self-test
