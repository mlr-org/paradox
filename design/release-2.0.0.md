# Paradox 2.0.0 active release ledger

## Status

**The active immutable package-facing candidate is
`refs/paradox-release/candidate-20260803T131049Z`, commit
`a0a9ff3e05b535068392e0c20442ad9794f3b824`, tree
`49079e12a816542fe8d8d6a0a2290a757a558b41`. Its complete nine-task
`release-core` foundation and fresh combined GCT/Valgrind/rchk memory gate
pass. Fresh dual-axis compatibility, mandatory documentation, exact-head
checks, and the independently sealed benchmark also complete every applicable
local gate. The broad-corpus and reverse-dependency coordinators retain their
reviewed semantic nonzero statuses as factual evidence; they are not relabeled
green. The first hosted companion for this candidate completed every job
successfully, but its retained old-Windows evidence failed closed because
checkout line-ending conversion changed the reviewed runtime-lock bytes. A
replacement direct-child companion passes local validation and its hosted run
and retained offline evidence are accepted. Hosted portability is complete;
the manual downstream/publication/release handoff remains.**

Coordinator `.local/verify/runs/release-candidate-a0a9ff3-r1` passed all nine
tasks in 3,429.2 seconds. Completion, JSON-summary, and TSV-summary SHA-256
values are
`f8f8430ab34ce3b84abf83b5229b42b74c74ef9b192a8071fad7ab78ac8831d5`,
`504484ba4c3cd5213b980f10d75356cad5633c85e42950dde8c7d23379ce4407`,
and
`f88a6f10436904435672a9049748dab7153d27118892b7560f4e3ff905cbcadf`.
It covers all four harness gates, 33 differential cases, API headers, GCC 15.2
and Clang 22 C23, native release on current R 4.6.1, and complete suites on R
3.6.3, 4.0.5, 4.1.3, 4.2.3, 4.3.3, 4.4.3, and 4.5.2. Replayable donor
`release-candidate-a0a9ff3-r1-native-release-a001` passes independent source
validation; its source-manifest/source-tree/modes-tree/completion/result hashes
are
`f9bc153e50a594902ecd67f240f2be274c08b4f9bde19ca79f9a19277b892dc3`,
`3b6246c007039a4a07a2132d521c64971155ee3feeddcbcdface25d4f309b1aa`,
`d7231b895a8e41ddce9d7f679b58f96c1e4c062484b97a02fb234cbbb16eccf1`,
`2a45e572c4f83b1e800927642f554db1d7a5e80aece9c6ca8ed2d7e57ca0101c`,
and
`1a2b43a43a03c8b87f9483d5a2b178056c486d678a92027fe84837735e328551`.

Combined run `release-candidate-a0a9ff3-memory-r1` passed all three modes and
the independent validator. Completion, result, memory-source-tree, modes-tree,
validator, and source-archive hashes are
`6e38ed22a4633d9145b5a5176a12f22c097947e3ea56fa8e9990c2e4b9036708`,
`80b582512c3b0b28c60cd5c0745bacf9545e32da31562b1865565b085f6af370`,
`577d57005e12ab21ad33cf06ae193790ff73620de81a21cff2f458ace6a74471`,
`a1bc3685a8456f7e42467c5c92e6ed2db2c18eefeb41f3dd7c9ddf87b7309363`,
`63b00bf51b22e65c35781589f1bef1a6cd7f4f74f0566e2a6b4cd58d189ad254`,
and
`0ed1d9bdb8750cd064971c25a0c72e1106eee976b62893ac2c2947f9a9ab9b62`.
GCT covers 111 routines and four hazards. Valgrind reports zero errors,
losses, or suppressions over 1,018 passes and ten reviewed skips. Rchk analyzes
1,305 functions and 201,585 states and exactly matches the reviewed 116-block,
397-UP/30-PB policy. Raw/semantic bcheck, empty maacheck, and fficheck hashes
are
`3afe281aa9aeb59a3ba468b60119b767ece80b0bd56ec38068b2d890bfc068cd`,
`c5a7396c584257e309d4738bac5dae13934764801c17612a683779834719a5fa`,
`e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`,
and
`742b60254990e82b45b4e33ead0b911946ade3f1751fb99f3401d90959bb6c88`.

### Final `a0a9ff3` compatibility, performance, and portability handoff

Final local compatibility and benchmark execution is owned by clean,
package-facing-source-identical tooling ref
`refs/paradox-release/validation-tooling-20260803T151943Z`, commit
`f7b3eff651ad747015a4d3372304f296eac661f7`, tree
`7749375e12300c6aa4258a76206ce082b96bd09f`. Fresh Paradox-1 and Paradox-2
preparations reproduce the same exact dependency-library endpoint
`3828cdaf0c767d89fa30ea595845d3eab9695dfdb80bee909a5b3fabcc31919e`;
both ten-package overlays pass current and retained validation.

Paradox-1 coordinator
`release-candidate-a0a9ff3-final-p1-focused-f7b3eff-r1` passed all five tasks.
All eight exact prepared heads built and completed their source-package checks;
seven have final `Status: OK`, and mlr3fda has only the reviewed environmental
`fdasrvf` cross-reference NOTE. Exact-check completion/results/manifest/seal
SHA-256 values are
`baf78a4eb90771c75b9b3661eaf28f287f8726ebe991e362268aa8a14ccdb87f`,
`48f1b58e8e9f655ffebf77a6b2598bfb01d4104089c881351bf80a2b45c5e564`,
`18ee1912763654a534a54ec95970b495dfcce8cd7fbb82e39f71dea8170c7545`,
and
`e93e7c97c290cab2d01cbb7235bbc1f657504335a428916a4a821907e97ea352`.
Coordinator completion/JSON-summary/TSV-summary hashes are
`c75fa0511601d1d653049b11ab4f87d28e3ecaa887dc88d0e9c8ddf0c8b66307`,
`7ecf4785b81c7b069deebcd1aba6c1faebea9dc8f94aa6c18734f95ce4e0f301`,
and
`246afeb23349366de384675283add74aa3e27cf64bbcc392774597b6f41d1dce`.

Paradox-2 coordinator
`release-candidate-a0a9ff3-final-p2-compat-f7b3eff-r1` passed ten of twelve
tasks. Its overall nonzero status is the intended factual aggregation: only the
broad repository corpus and CRAN reverse-dependency tasks are non-green, while
every harness, preflight, overlay, exact-head, and mandatory-documentation task
passes. All eight prepared heads again build/check successfully; seven end in
`Status: OK`, and mlr3fda retains only the reviewed `fdasrvf` NOTE. Exact-check
completion/results/manifest/seal hashes are
`4070d28e5b718d2f015840f9176bcf6940ebbc715f1e54d5101481d394b5ee11`,
`577220d884db6c79e1ff8451fe1ec419ba859ecce6a879433512858d71797f69`,
`642e03e2ffda3c7a79a2f91fd64ddf656dfdeebd29d18e78e8ecc5a9c060b664`,
and
`bd20d9d3131c50dbf7ca980381f7e03ae86d479c69756546a67ed0a713388f9b`.
Coordinator completion/JSON-summary/TSV-summary hashes are
`efa95075775c5edfa997c85f2fadc7544fc2658f5a89ac3911ae267f6e41cecd`,
`3a80d8d950a9cdad45810bb90ee43a827290e6a5749561a8c91773925a22d5eb`,
and
`b38b9e54e0e12cf7cb1d979cb6fadc195a822e827f9363dd23de4773db0217bf`.

The broad corpus completes 20 of 28 exact repositories. Its eight non-green
rows are retained reviewed upstream/environmental results rather than Paradox
defects. Completion/rows/manifest/seal hashes are
`190fa978c27655774353b961a81bbcb343585062b7c33cdf40dcd7b30338a9bb`,
`c8cff5a0352b9e096dc246907d739bd7cf05139fc0241fdc1b2695b94f71ddf3`,
`fea401a5addc39a3e1f1d397202498b030113a7cd0086867c1377c86cd26fb8c`,
and
`f9a1d04f5ed72bb73d5901652074de57bd5762017496913fb28e75a0be11a5cf`.
The retained mlr3extralearners row is exact commit
`f596c7313cdf280075f033de90f42cf4f310c098`, tree
`ab14db5b52cd449076aef3b3ffe6c48609b9fa5d`, source-archive SHA-256
`f60ff43922d5d8e1c93a83752c8c3d4eac899a302ec31424a764d6867b704ac1`.
A diagnostic focused execution in the same pinned worker and exact candidate,
dependency-library, and isolated-environment boundary selected only
`tests/testthat/test_prioritylasso_classif_priority_lasso.R` through the same
repository child: all 19 expectations passed without a crash. Its process
exited with status 1 only in `teardown.R` while trying to download `uv` in the
networkless/no-Python worker, confirming the already reviewed environmental
boundary; this diagnostic execution is not sealed evidence. The exact
mlr3resampling row at commit
`3da4ef08f8b98ba3b28478a68d0ad73d4114a186`, tree
`749e43088064ba3cad0f1a109f6142e9abb0e3fa`, source-archive SHA-256
`dc224b5dd11730c51908154feca04372fdb96deffb06ade17faec965bebb8e0d`,
and the same dependency endpoint as the preceding candidate timed out at 3,600
seconds after 80 passing expectations and 26 successful blocks along the same
known `FutureLaunchError` trajectory. That is a reviewed environmental timeout,
not a useful retry or a Paradox regression.

The reverse run completed all 22 exact rows without timeout, OOM, missing
worker, interruption, or transient failure: ten pass and twelve retain the
same reviewed unadapted-release or external/environmental outcomes. Its
completion/results/acceptance/waves/manifest/seal hashes are
`104e087a3ab599c9952193642803334cdb211ffcbd0ccb008a66fdbebaeca86c`,
`cfcf6f133dc1e68d593af6fc21399c255bbde6224d16d623328c1d72544cf32c`,
`b70cb6cfe7088f8c90c8ff056bd42e2658c8cda3b2899aab632d97fc289f6d3f`,
`4cbac7fb16ac3298d53462ba78086f584faa3e45c27588042ac5eaa7d24c234d`,
`d288e297416ed42d463bee39343625fe72111b296db6363619289c078afba83e`,
and
`341405a5e995dbdbc7d9391da73168e4d16c8cfd0632543dce34713c0e38c55b`.
Current and retained verifiers accept both broad stages.

All seven mandatory documentation conclusions pass, and 14 of 17 rows pass
overall. The three advisory exclusions remain the two legacy-gallery rows and
the full-book render. Results/manifest/seal hashes are
`8a0532d1303d8b5486e1361c07a0925e5c32897068e47e8af1b5373196d12de4`,
`d6033fdc4681d9112316046ffc3d11874001cdecbd65b2a453aada9c955aff91`,
and
`688e5aa0c4b9776faa0f5dd567f4c1878f9cf3bd68da309fb06b1e159c070c34`.

First benchmark
`.local/benchmarks/release-candidate-a0a9ff3-final-benchmark-f7b3eff-r1`
is diagnostic evidence only. It recorded 78 passes, three bounded marginals,
and one `shadow_values_live` median-time failure: its 5.724 ratio narrowly
exceeded the 5.5 integrity budget while its upper-quartile ratio remained
within budget. It has no manifest or seal. Completion and decision-table
SHA-256 values are
`7f2d62b2370acde29101068d54472868b1446cc3c4db6c1c0fe18ae5e8e5c47b`
and
`229c6d991453cd1d5586bb5baabeb04afc9d77771aed1f94e468a2d6d6570e41`.
The installed candidate DSO is byte-identical to the accepted `f27776e` DSO
(`704f2b07e5ab3852b0f0c51d088d76b27901f5f6aa5e31ef2ad2fcd210e17faf`).
Four fresh-process 1,000-sample A/B and B/A diagnostic blocks measured
`a0a9ff3`/`f27776e` median ratios 1.0694, 1.0128, 0.9633, and 1.0939, with
identical allocations and an order-dependent sign. All adjacent Shadow rows
moved together in r1. This bounds r1 to power-state/scheduling noise and
justifies one complete unchanged-policy rerun while retaining r1; the focused
diagnostics are not sealed release evidence.
Fresh independent run
`.local/benchmarks/release-candidate-a0a9ff3-final-benchmark-f7b3eff-r2`
passes all 82 policy rows with 79 passes, three bounded marginal reviews, and
zero failures. The marginals are `design_transpose_plain` allocation,
`shadow_values_live` timing, and `collection_values_nested` allocation.
Current and retained independent validators accept the sealed stage;
completion/manifest/seal/decision-table hashes are
`f2a9898d2b107999da63417ccd8255f114665b804f72953ced2d7397cf2b6010`,
`50b2dc99c6609d21af923ec8c8b6daaa87dc5c3a25723ecc00aa8da3cb5aa2ad`,
`b56eb4b02f9bc60b30e5a8cfaf992b78b03f95be2b62a146b9639f3734b1a016`,
and
`a45d9ed8a91e7344616428b9e6235f7c1b2412ba80ea973622148d7d4067c0cc`.

Initial locally validated direct-child companion
`refs/paradox-release/portability-harness-da5a500`, commit
`da5a500936d00f7bc4c44989258d5bc385082252`, tree
`ad699de13d8f2df1b08530db4cf3a6a08d3a7693`, with tag
`paradox-2.0.0-ci-a0a9ff3-harness-da5a500`, changed only
`.github/workflows/r-cmd-check.yml`, SHA-256
`6e09fa7d068886c05c0d1643b49fe8f48cbd7dec49a7e1ee757c0649088ebb18`.
Hosted run `30853585319` at that exact head completed current Windows x86-64,
macOS ARM64, exact Windows R 3.6.3/Rtools35, and the aggregate required-job
gate successfully. Offline evidence validation nevertheless rejected the run:
the reviewed runtime-lock entry is exact Git blob
`5e9fb484b63cff6ee51ab2101dcaa37defd0e603`, with 7,776-byte raw SHA-256
`9007e3a2d7eecb1057bf9610a2f2ffacf617c224b9aeb9b91bd1ef5ae85f59c5`,
whereas the retained old-Windows artifact copied the 7,819-byte CRLF checkout,
SHA-256
`ff1fa9d5b65a52fc843e25d1de1e2429128cc114a9541f20e92c772f07ae4d4b`.
This is deterministic harness/evidence-representation failure, not a package
failure; the green hosted statuses do not close portability, and `da5a500`
must not be retried unchanged.

The repair and its independent validators are frozen at clean,
package-facing-source-identical tooling ref
`refs/paradox-release/portability-tooling-20260803T221658Z`, commit
`3f48d33821712d3a79626dc4041a792d3efac0c9`, tree
`5797a2bfb7c24d9e1d8c190af1908ac636786156`. The active locally validated
replacement is exact direct child
`refs/paradox-release/portability-harness-00a24cb`, commit
`00a24cb3a094f08e486e4271d673a13f18df0a90`, tree
`96fc9ad4fc7d9607f9222bfa2f50b3a19a0ff0b0`, tagged
`paradox-2.0.0-ci-a0a9ff3-harness-00a24cb`. Its sole changed path remains the
package-excluded `.github/workflows/r-cmd-check.yml`, SHA-256
`bb17199b4c6621bcc427961e499e9fc88e00407e48b73789de0447c9c57f1e40`.
The old-Windows identity step now authenticates and installs the exact raw
candidate lock blob before the unchanged helper consumes it. Local structural,
deterministic-renderer, adversarial, offline-verifier, and actionlint checks
pass. Hosted run `30941929181`, attempt 1, completed successfully at exact head
`00a24cb3a094f08e486e4271d673a13f18df0a90`. Its four exact jobs all completed
with conclusion `success`: `92102264745`,
`windows-latest / x86_64 (release)`; `92102264822`,
`macos-15 / arm64 (release)`; `92102264982`,
`windows-latest / x86_64 (R 3.6.3 / Rtools35)`; and `92110378655`,
`Verify required check jobs`.

The three retained artifacts are ID `8906413634`,
`paradox-2.0.0-portability-windows-latest-x86_64`, 4,529,716 bytes, SHA-256
`29f0dc248acf9b2fa24e00015ee29f8e7a1d76707179dd9f9a5271f9e24803b1`;
ID `8905968872`, `paradox-2.0.0-portability-macos-15-arm64`, 5,956,865
bytes, SHA-256
`94f7f9331e1d0fc7e835368d16ed192d085b9d6786355bf5c13840f6090effbf`;
and ID `8905625056`,
`paradox-2.0.0-portability-windows-r3.6.3-x86_64`, 3,377,151 bytes,
SHA-256
`d030690af4a9d6a5083a411f5901eeac938f8ab287164c3c7690cce43a359b82`.
The old-Windows artifact retains the exact 7,776-byte candidate lock with
SHA-256
`9007e3a2d7eecb1057bf9610a2f2ffacf617c224b9aeb9b91bd1ef5ae85f59c5`.

Accepted evidence is
`.local/ci/r-cmd-check-30941929181-r1`. The current verifier and an
independently relocated retained verifier both pass, and the accepted
directory passes a post-promotion verifier replay. The exact
`verifier-acceptance.log` receipt SHA-256 is
`2c987700bb09171b23e8625bda2a7f63d5a6deba8c2bf7f5f38ed7331e8d1405`;
its six checksum manifests cover 1,357 exact members. SHA-256 values for
`ARCHIVE-SHA256SUMS`, `ARTIFACT-SHA256SUMS`, `EVIDENCE-SHA256SUMS`,
`JOB-LOG-SHA256SUMS`, `METADATA-SHA256SUMS`, and `VERIFIER-SHA256SUMS`
are respectively
`72ca72c623fc88ef7483f1612b2a64bc9b52a4fc23523e5c073bd7a3b508264c`,
`a634ff28dd9e575a0410a5f256245812b0580f7bf7146ac96c528b5f63b5097f`,
`f7afbe507a2a300d6ec76415aa40e8ff6c79d4100edfc8ed9c3481dacf3dab9c`,
`14a1e22aeff184bc813c37b64406ddfdfb5307391b83069255eed55b74eece7a`,
`1d5f2d7f2b90399d2344dbb2d8f92a385e730449d0370f9f8bae7c5b0e1a2c6b`,
and
`5d1d9944e4c4963f4b535b5e169d516ce02459fd93ff0bad3fd612a8969557af`.
The earlier
`.local/ci/r-cmd-check-30941929181-r1.staging-failed-a001` attempt stopped in
its absent-target guard before extraction or promotion because the guard did
not handle `Sys.readlink()` returning `NA`. It was never reused and is retained
only as failed harness diagnostic material, not as accepted evidence.
Hosted portability is complete. The manual downstream/publication/release
handoff remains.

### Historical `f27776e` acceptance record

The exact coordinator is
`.local/verify/runs/release-candidate-f27776e-r1`. Completion, JSON-summary,
and TSV-summary SHA-256 values are respectively
`d43233b72252559f85e22886c714663cdd5bc2cd6596776fa212d5a0db9448b5`,
`24dcbb1acc8c313491772f39b6d03490ef47beecdc1b6d4b1e8e45f076b725f1`,
and
`b8f7c17e05190be64e3b745c7f6d64d9ec15cecf5725ca1ba1cc035ab701925b`.
Replayable native child `release-candidate-f27776e-r1-native-release-a001`
passes independent validation. Its source-manifest, source-tree, modes-tree,
completion, and result SHA-256 values are
`67ce64d443709ebf2f5808a2a09232f375b7df0a8f311a6882ab1447e5558294`,
`dc807b317f8d5a89d87c554c3d39c6991285711f1d0ee48eae3c80494f7e3277`,
`c01a064b3457614090fcab6cfad765544e494299bbc3ff7cf5a180896f207323`,
`4d3dddf2c1bc5939437a64e702297203198e39e041daba85e6a6532cd111df53`,
and
`b46b47fc3e4196d5fecfc3f8d706579f6597de7de243595df59c25f385ea3d3c`.

Discovery `release-candidate-f27776e-rchk-discovery-r1` ran bcheck,
maacheck, and fficheck successfully and failed only at the intended old-policy
comparison. It analyzed 1,305 functions and 201,585 states; 116 blocks contain
397 UP and 30 PB diagnostics, and fficheck records 111 routines. Three
independent reviews found no C defect. The sole normalized addition is the
benign address-taken `classes` out-parameter in
`scan_unchecked_value_leaves()`, still owned by the protected transaction
snapshot and unused after the scan. Raw/semantic bcheck, byte-empty maacheck,
and fficheck SHA-256 values are respectively
`35d5ad41f7fe4bcbe62d8848759dd694e00be29bf06c2109852d608d4ae304f7`,
`c5a7396c584257e309d4738bac5dae13934764801c17612a683779834719a5fa`,
`e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`,
and
`742b60254990e82b45b4e33ead0b911946ade3f1751fb99f3401d90959bb6c88`.
Independent generation and cross-validation produced policy/block/rationale
SHA-256 values
`3ca2416f9d0920850431381d35fc0ce00f8d2fe5b4e08fe98d85f86e47dfd806`,
`5ae729f8d3b07bd471050a81f793a862fda59e1a4457483fb66618e27acc250a`,
and
`c9e94a9f49b5570838df94fba7f45ef870999b78d03b7b4824412dc34645d8a4`.

Combined attempt `release-candidate-f27776e-memory-r1` passed GCT but stopped
in Valgrind prerequisite preflight before either Valgrind or rchk ran. The
fresh and sealed 30,917-row toolchain receipts differed only because
`.local/toolchain/lib` had mode `0777` instead of `0775`. Root cause was a
compatibility self-test's external directory symlink: R's forced recursive
scratch cleanup performs a pre-unlink `chmod` that follows the link. The
fixture now avoids the external link, supplies the copied scratch Git's
libraries through explicit `LD_LIBRARY_PATH`, and asserts that the live mode
does not move. Restoring `0775` reproduces the sealed toolchain state. This is
harness/environment diagnostic evidence, not a package or analyzer failure;
a fresh donor and combined run were required.

The repair is frozen at package-facing-source-identical tooling ref
`refs/paradox-release/validation-tooling-20260802T203920Z`, commit
`c20c1a3e7bb459757d57038c6eaa89daa6d9082c`, tree
`c8f8ca6e697b4b6bd46761ff408de1e4376c5eab`. Donor
`release-candidate-f27776e-native-policy-r2` passed all six static/focused
modes and independent validation. Its source-manifest, source-tree,
modes-tree, completion, and result SHA-256 values are
`b4216e4b508fb7cb0776ced8cd0c1ee7fa19df880d89106d4625ffb819af95fa`,
`81ada3c322292685931da76f8791ced8ee1275f7ae7ad035ab4a083c87816ecf`,
`4735b3df6228d1cb13ee1cc9dfb0ec3900cc934228f68ec00a0cc159582506a3`,
`4fe4b6e06de316fcbf8d91d6ca6f3da365a8f5f691359230137b7d9743cc0285`,
and
`56b25164c1497e29f09d991fe5527620e9a6bfed14a63e20f63187f41f082cac`.

Combined run `release-candidate-f27776e-memory-r2` passed GCT, Valgrind,
bounded rchk, and independent validation. Completion, result,
memory-source-tree, modes-tree, validator, and source-archive SHA-256 values
are
`62e81f915f39b9500a5a964f99cc2d384765f96d6571185584fe1fe0660b44dd`,
`b23c6bb8a0d521ea4366d3914eb24529454fea5b2d00a265ab3e36373d499f1d`,
`18969ef1367133958dc4271c8dc48468f100b2853458e0af07be04751d515877`,
`98943dfffcb27025d3d098052ddf87c0cadc0cff5b06866aea1fc580612d7344`,
`63b00bf51b22e65c35781589f1bef1a6cd7f4f74f0566e2a6b4cd58d189ad254`,
and
`0da3db695f7182fbf6ce436d52f12f3de7f29cb109b582682054484b3b7e5177`.
GCT covers all 111 registered routines plus four hazards. Valgrind reports no
errors, loss, or suppression; eight files and 141 blocks yield 1,018 passes
and ten exact reviewed skips. Rchk matches 1,305 functions, 201,585 states,
116 blocks, 397 UP, and 30 PB. Its raw bcheck, semantic, empty maacheck, and
fficheck hashes are
`3afe281aa9aeb59a3ba468b60119b767ece80b0bd56ec38068b2d890bfc068cd`,
`c5a7396c584257e309d4738bac5dae13934764801c17612a683779834719a5fa`,
`e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`,
and
`742b60254990e82b45b4e33ead0b911946ade3f1751fb99f3401d90959bb6c88`.
Combined-memory acceptance is complete for `f27776e`.

### Final `f27776e` compatibility, performance, and portability handoff

Final local release evidence is owned by package-facing-source-identical
tooling ref `refs/paradox-release/validation-tooling-20260803T023640Z`, commit
`f711c67dadd24fec80779a319d40f7032bed7e78`, tree
`c6399e5e18a749ad0c647fedd602575a0a45c09e`. A first focused Paradox-1
attempt stopped in cheap harness preflight because one self-test still named
the preceding candidate. Commit `f711c67` corrects that stale exact assertion;
no consumer or package ran in the diagnostic attempt. Fresh preparations for
both axes reproduce exact dependency-library endpoint
`3828cdaf0c767d89fa30ea595845d3eab9695dfdb80bee909a5b3fabcc31919e`,
and both profile overlays pass current and retained validation.

Paradox-1 coordinator
`release-candidate-f27776e-final-p1-focused-f711c67-r1` passes all five tasks.
All eight exact prepared heads build and complete source-package checks. Check
completion/results/manifest/seal hashes are
`da3ea0f6ce24639cd9af6d1d83d1f32f3eeed138ebeedd5c9f4851d3e90865ca`,
`2e8595a74aa9df7adbd1cb0998b0a25aa0bcc220dfea91189fc49fe0a29cc83a`,
`d909980ec907143229a8acced261a6f357fbcf55e059ce03e7a4d20d1ced0e1d`,
and
`c45d078095689941c41ca5e73cb532d85b84a14e7904e73f67cd96bbfb13c5c6`.
Coordinator completion/JSON-summary/TSV-summary hashes are
`b9dd1d97fb428ad4a8a1466459eb849df803397a564a6dae061d5f3d88a8ed6c`,
`c80505e26aa5c2b8d560c50f91c18d1c0522c73fc2b38e063c23d3beee770603`,
and
`45c08e627c993d4d07d2e7a234849326d223d651cb865784115fa5ee6de01ebe`.

Paradox-2 coordinator
`release-candidate-f27776e-final-p2-compat-f711c67-r1` passes ten of twelve
tasks. Its two nonzero semantic aggregates honestly retain reviewed failures;
there is no harness, timeout, OOM, or candidate failure. All eight prepared
heads pass; seven checks end `Status: OK`, and mlr3fda retains only its
environmental `fdasrvf` cross-reference NOTE. Their completion/results/
manifest/seal hashes are
`f3a000732a1ff786d0a2bcf4191015d2b065f1fe0dd2b0037fc3dc5e30f38159`,
`c5fe3fdc8a2d0bed4d7bd4eafcef3a1af28af1ad2e81708c90437ff72aa5299a`,
`e7c356cfd18cce1c6892c22f0c3c4c278dcf4d0e32fcda18d6f1be0ed22f95af`,
and
`6395aac9a706507851191b80227e7063306ce686548ac9cf32f93764aafd5cb8`.

The broad corpus improves to 20 of 28 exact repositories. Its eight
non-green rows are reviewed upstream/environmental results; mlr3forecast is
now green, while mlr3extralearners completes rather than timing out and exposes
only absent Python torch/botorch/gpytorch state. Completion/rows/manifest/seal
hashes are
`6fe7469db2b6e5d8a6b3f5ad1efcd4f3f49a873b188a30a25f7da8e2f8c87b3c`,
`057be5a172e38ed67bc1f88fcfc6c94c2478fd9e9329c82a95488a32d845079d`,
`7f8ea5024ad704bc1dac945120cf720eebf53a5d5e555fec8127a9cafe2c5628`,
and
`dcd1db9214529ff569387ffc19303c82460b289119670f6cb21ae9e4c8e2f423`.
The reverse run completes all 22 rows without timeout: ten pass; twelve exactly
reproduce known unadapted-release or environmental failures. Completion/
results/waves/manifest/seal hashes are
`06298c91d4a46060dd886feb598100c49ead1ae6ee7446080261295f33686d32`,
`3a34f6590c672834d2c216c28eb99b3e3ab7a0bdb0bf8ef264b05301a7b3834f`,
`969369c94704514b9eacf14d307ee3b47fd312300c960b215586239a7482b0cc`,
`4c6797d39143311229350c7281219a5bfdf9c2e8af68dc138b827e230a8c9b87`,
and
`4a9546bcb5301807350e9f77b9711619e45cd0128b4118da981791ef12a2bfd7`.
Current and retained verifiers accept both stages.

All seven mandatory documentation conclusions pass; 14 of 17 rows pass
overall, with only two legacy-gallery rows and the advisory full-book render
excluded. Results/manifest/seal hashes are
`cddf1f4ae76f3a094731fa20ee78169c2f62961f3ab2a16578edb9feec4b0166`,
`40caee4c34431a00225255516ed388e8c99f8db96f67a429e2ea95816daa963a`,
and
`9a45219b40271368bbe4d81e7d692e47af530fcd8c3698a9e3816fd8a30cbb32`.
The compatibility coordinator's completion/JSON-summary/TSV-summary hashes
are
`853e059f059cd347ac58358aa0067a677faa3d8f6826f42e3f0a3cdb5e271711`,
`4534cea0e2158092388a61b8151b1174229b83cef5a597131f27f01cea02e7fe`,
and
`5a1c82014f9e79fc1ef316d1eb5e9626c72f2a09810be4d5b096397465b7c294`.
Do not call that coordinator green: its two aggregate nonzero statuses are
part of the factual evidence contract.

Sealed benchmark
`.local/benchmarks/release-candidate-f27776e-final-benchmark-f711c67-r1`
passes all 82 policy rows with 78 passes, four bounded marginal reviews, and
zero failures. The marginal rows are `design_transpose_plain` allocation,
`shadow_values_live` timing, `collection_values_rich` timing, and
`collection_values_nested` allocation. Completion/manifest/seal/decision
hashes are
`ce90fc5bd7468955ec57615e89354b82e4e1484a9936a2b51b92c11b7e743c66`,
`5cfb9e69d23449b8cea85d7abbe9eb59a0f810a7fbc6a9e38b6d1ac5fc2bb571`,
`4af296b0a895470efb927c6f39f8de32872749029af14a272d4c3d05ccaab524`,
and
`e72a59f826d749231da330f5ffbc2d6bddc0ee703d8c1750858227f8af7610cd`.

Final portability-validation tooling for superseded payload `f27776e` is
frozen at
`refs/paradox-release/portability-tooling-20260803T103512Z`, commit
`812e5abef05c86f743425f6d984fb146c2827434`, tree
`e99604f05c10db57406773383d2e0a73746a139c`. Its historical direct-child
companion is `refs/paradox-release/portability-harness-582eba8`, commit
`582eba86e7a05428f63608272c1c6c6e11a894f4`, tree
`e5ba13f476b4997fea4dbaf5d60369a058ad024c`. It changes exactly the workflow
and old-Windows installer helper below the package-excluded `.github` and
`scripts` roots. Their SHA-256 values are respectively
`a7d55d3f3df1543753fbea37424dd3702d8c8de187cb213354f5580db4f35f44`
and
`283e3450e47337f3fc121d6e103c1c0a0ad66ab4cab63b8c42caa57ab174381b`;
the helper is exact `100644` Git blob
`3b420d542dc5a1ae5506380543159cdea84517fa`. The workflow proves the companion
has candidate `f27776e` as its sole parent, admits only those two paths, and
authenticates that helper entry before execution. All local structural,
PowerShell-parse, deterministic-renderer, adversarial, evidence-verifier,
documentation-economy, and actionlint checks pass.

Hosted run `30793059118` used failed companion `198e838`; current Windows and
macOS passed, while exact Windows R 3.6.3 stopped before Paradox because
top-level `Rfe.exe` lost a multiline `-e` operand. Hosted run `30803703541`
used now-superseded `ff3b510`; current Windows and macOS again passed, while
old-Windows job `91654062556` stopped at `PATH does not select exact R 3.6
x86-64 executables` before R, a compiler, or Paradox ran. Missing old-Windows
receipts/artifact and aggregate completion are consequential. Retrying either
immutable companion cannot test its repair. Run `30807910809` against
`582eba8` then passed current Windows and macOS and completed the exact
R 3.6.3/Rtools35 build, install, load, smoke, and bounded package check. The
old-Windows wrapper rejected deterministic `Status: 2 NOTEs`: the expected
missing-Suggests NOTE plus an Rd-cross-reference NOTE for `lhs` and
`spacefillr`, intentionally outside the seven-package ABI closure. This is a
harness-policy failure, not a package failure, and retrying `582eba8` unchanged
is useless. The replacement sets documented R 3.6 switch
`_R_CHECK_RD_XREFS_=false` only for that bounded ABI check, preserving the
exact one-NOTE contract while complete local/current checks own documentation.
The hosted old-GCC log also exposed `-Wmissing-braces` at the upgrade-walker
zero initializer; the replacement uses a C99 designated scalar initializer.
At that historical point the replacement source was unfrozen and required a
new candidate and direct-child companion after focused gates converged; active
candidate `a0a9ff3` now closes those source obligations, while its hosted
companion history and active replacement are recorded above.

### Historical `4e549f3` acceptance record

The exact coordinator is
`.local/verify/runs/release-candidate-4e549f3-r1`. Its completion,
JSON-summary, and TSV-summary SHA-256 values are respectively
`c492eeb65a578cbcc868e09d7c222788a524f6795328c6d0a600e84730c16406`,
`c1e5bb86dfbfb9f56d7ce64667f47d65dbd03cf72a2a33d3c27652bae68c030c`,
and
`59d95159bf1f6bdbb50f07cf1087ed0c2f0dc604d1102a81c53c5364f845f855`.
Replayable native child
`release-candidate-4e549f3-r1-native-release-a001` passes independent
source-run validation. Its source-manifest, copied-source-tree,
copied-modes-tree, and completion-content SHA-256 values are respectively
`2362acd1d5748792c1e7b02040c02b11da0309aef325c611c798fa62ff049e88`,
`5eb7c1e4961bb46b632d227792414c533103648a98d6cf9cc3b20956dbf06352`,
`18697cb2b06f0ebf43cebec847fc375c9833438d623951d1ad9eb7ac7c25a1a7`,
and
`8e9ad5dfa1c22d84629669833b6aa2c8cdacb71df22f30dcfe7e703412ee15b5`.

Isolated discovery `release-candidate-4e549f3-rchk-discovery-r1` completed all
three analyzers and failed only at the intended stale-policy comparison. It
analyzed 1,288 functions and 202,140 states. Its 115 function blocks contain
396 UP and 30 PB diagnostics, and fficheck records the exact 110 registered
routines. Raw bcheck and ordering-insensitive semantic SHA-256 values are
`cda7598e591bcfa7b3866acdc09530d24dc1643de17fff144056b46cde76e78f`
and
`2c1493d88d28866e56c52c7640fad9af791cacbe893ea57b23b72f87be110aa8`.
Maacheck is byte-empty, SHA-256
`e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`;
fficheck SHA-256 is
`26bfc1718868d77f8f1d477c0d60312cfc707d1cebc8afd433079bf6f51918a0`.

Three disjoint, independently performed source-review partitions covered every
observed block and found no C defect. Independent policy generation and
validation produced exact policy/block/rationale SHA-256 values
`e2e7ccc9cc225240986bcb99fc6bd64f8b828924d4aaf0d5765a8a847c346087`,
`88363b5a2c173b378ebcdf2f6e3f05f8b7234240b9f04fc63b343871b1d21067`,
and
`c9e94a9f49b5570838df94fba7f45ef870999b78d03b7b4824412dc34645d8a4`.
That exact source-bound policy is now the checked-in policy. It and the pre-run
release ledger that supplied the first donor snapshot were frozen in the
package-facing-source-identical validation-tooling ref
`refs/paradox-release/validation-tooling-20260801T112017Z`, commit
`0507daff5cbd208781172b0f35ad405975c342f9`, tree
`d36e26e5881ba443e09de79c5990f0fcb6c34f84`. Fresh post-policy donor
`release-candidate-4e549f3-native-policy-r1` passed all six static/focused
modes and independent source-run validation. Its completion, source-manifest,
copied-source-tree, copied-modes-tree, and result SHA-256 values are
`19bffadbcff762dedea96d4468a7f1481dd07b3be512d4a1fa1b24dfb81c56d4`,
`3fdf8708a0dc94e5835238611ec179e1e6dc6b7e9d9c7329043250ca321c2f02`,
`02ba4e8d2c0732d6a6e4413ea855c4257efeb6d11ed88f0acca471e3b34724ae`,
`9baec8705159a41d00e7224b379822553f6709bf64efaa9711eb7d45621b06fc`,
and
`5c116f81689bf905b4ceafbe0cbda1c1d41c4b64488f5f00e3dbea1ed5c3192e`.

Combined-memory run `.local/checks/release-candidate-4e549f3-memory-r1`
executed GCT, Valgrind, and bounded rchk successfully. Valgrind's baseline,
probes, and analyzer have zero errors, lost blocks, or suppressions; its
focused ledger records eight selected files, 852 passes, and ten reviewed
`skip_on_cran()` scopes. Rchk matches the exact 115-block, 396-UP, 30-PB
policy. An independent evidence audit then found that the Valgrind scope
receipt incorrectly called those ten scopes “eight,” confusing the selected
file count with the scope count. The trusted ledger and executable evidence
are correct, but the false receipt makes r1 diagnostic rather than release
acceptance. The package-excluded harness and independent validator now bind
the scope count to that ledger instead of hard-coding it. A fresh donor and
all-mode run were required for the corrected harness; r1 remains semantically
clean diagnostic evidence and does not own acceptance.

The correction is frozen at package-facing-source-identical tooling ref
`refs/paradox-release/validation-tooling-20260801T124036Z`, commit
`3902d5bf0bedcef5a39f266978f371a8cc5640b7`, tree
`ce0b22d8276a8524efe47451d7d76a824d30031e`. Fresh donor
`release-candidate-4e549f3-native-policy-r2` passed all six static/focused
modes and independent validation. Its source-manifest, source-tree,
modes-tree, completion, and result SHA-256 values are
`adccc238e79a04ff83d3e1394fbb4fdce9e3817b270839dc1bb0a87a79bb8698`,
`f5b0f7cf3a1230938b0dd59a8910e893f2d506a6b5f0177692ab97850c0970d5`,
`c10d5aa4a68a35365175836949dab466fbe9ceb4ddf5602d5b0a05bbb102b0ea`,
`a4c812fddffc1dbd66f26ba37241b2d7d994daad10359454e10796b605a3a747`,
and
`40d934a672ac66bb221db3fca43e45889972a9e5e964774c31ed3753baaf9bb4`.

Corrected combined run
`.local/checks/release-candidate-4e549f3-memory-r2` passed all three modes and
independent validation. Its completion, result, memory-source-tree,
modes-tree, independent-validator, and source-archive SHA-256 values are
`5bdeda5338c840d73301a6b08f142692517c933476a5f1e77edab877bda7bb62`,
`508dadf355230d03ce8678c0e666b61cc2022b4892c503c0b7815beace6c0cda`,
`964d6133d7e0ac2595d8df60693bfd0062b3706d384128a174b5f49f3436a228`,
`754665b8f1383fab02b7c38ae3b357d0799ed6576e5f7cf69205ac8dbcc63c70`,
`63b00bf51b22e65c35781589f1bef1a6cd7f4f74f0566e2a6b4cd58d189ad254`,
and
`1c137df1ccc07e83b7b11a71b6e564baf2d53d9f3907a72f74fc6b08e704387d`.
GCT covers 110 registered routines plus four hazards. Valgrind reports zero
errors, losses, or suppressions; eight files and 128 blocks produce 862 exact
expectations, comprising 852 passes and ten reviewed skips. Rchk analyzes
1,288 functions and 202,140 states and matches the exact 115-block,
396-UP/30-PB policy. Raw bcheck, semantic, byte-empty maacheck, and fficheck
SHA-256 values are
`d0e55ca0b0b46a53e5f551ebe1d84786a235d74199cbd1c4093709d7bb79aac1`,
`2c1493d88d28866e56c52c7640fad9af791cacbe893ea57b23b72f87be110aa8`,
`e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`,
and
`26bfc1718868d77f8f1d477c0d60312cfc707d1cebc8afd433079bf6f51918a0`.

The final candidate-owned compatibility runs use the converged inputs below.
Fail-closed staged CRAN refresh
`.local/compat/cran-refresh/release-refresh-20260801-r2` projects exactly 37
tested direct `Depends`/`Imports`/`Suggests` consumers from index SHA-256
`ffd50ff8e4d3f0278a77c352585d74af2994f4e4b0c9375155e3a3d6549737c8`.
It adds only reviewed `mlr3forecast`, advances eleven package versions, and
produces exact inventory/snapshot SHA-256 values
`83f266f83d9e917c0f609d6f54e6d61c552ae4c76f63d3f91a2b6626682a9066`
and
`34c55d30282f1954e5b408110e478524ec624d971d74cee4934d05f3e546c991`.
All 37 canonical archives pass offline authentication; eleven superseded
archives remain preserved outside the canonical directory. Completion and
seal-file SHA-256 values are
`afaddc43c615b76ac105d7a6da254e045d7746846bf822b56355582a8adeb574`
and
`e4883da9c04ec41d380b77848f462b3372d8fa9e8a5efe1678e8dfc1a98a1ee9`.
The preceding `r1` stopped before projection because CRAN's new versioned
Recommended rows exposed a deliberately fail-closed duplicate-shape rule; it
is harness diagnostic evidence only.

The refreshed dependency profile also pins Rush commit
`939886b43d5e48afacf2f0e1b06a45ab3c006e19`, tree
`ca34e7a22145816437161e79a84b7b7a0eb1a0f4`, as an exact transitive provider
for the prepared bbotk/mlr3tuning heads. It is not a Paradox consumer. The
schema-5 producer and independent verifier authenticate each run's Git archive
and extraction, map those exact bytes to one commit-keyed read-only canonical
source, and admit only pure-R exact providers without configure or cleanup
scripts. One nonblocking owner-authenticated lock below `.local/compat/`
serializes both the canonical registry and shared dependency library; an
interrupted retained lock is never reaped automatically. Direct
`R CMD INSTALL --without-keep.source` uses the commit instant as a fixed
`Built` timestamp. Retained evidence binds both source trees, method, metadata,
absence of run-local absolute paths, and unchanged full installed-package
content. The fixed private install HOME/TMP/cache/work directories must be
exactly empty at both boundaries, and a canonical generation must remain
retained while its evidence can be replayed. The final P1/P2 boundary must also
reproduce one exact full shared-library endpoint hash. This is necessary
because multiple Rush commits share version `1.2.1.9000`. Schema 4 keeps its
old parsing branch for replay with matching historical tooling; the current
verifier still rejects a stale retained harness hash. New runs emit schema 5.
Legacy schema-3 evidence retains its old selection contract.

The schema-4 diagnostic preparations
`release-candidate-4e549f3-final-p1-55e8403-r1` and
`release-candidate-4e549f3-final-p2-55e8403-r1` both completed, but cannot seed
candidate installation. Their shared-library boundary was
`6b8682ac796c7c1d24bad544fa210db7eb031ba05558fd69a4e861a98942ba7d`
to
`fb8a332efb981da38181db83c1ed05a4b574be8df31e625953980267c6bd955a`
to
`1ec7d01daa2cd4be985c7157cbc86580651488571edb362e97b5a85d93e4bd55`:
Pak embedded run-local Rush provenance and build times, making the P1 proof
stale before P2 completed. They are harness diagnostic evidence only.

Final broad compatibility and documentation execution is owned by
package-facing-source-identical tooling ref
`refs/paradox-release/validation-tooling-20260801T182922Z`, commit
`b650735804449ef6c64b8d042a56907faa6ec980`, tree
`03fe5119ece241839d4d429678e4b2baa5e16df1`. The repository corpus completed
19 of 28 exact rows; all nine retained non-green rows are reviewed
non-Paradox upstream, optional-runtime, environmental-dependency, or bounded-
timeout exclusions. Completion/rows/manifest/seal SHA-256 values are
`22a513d6616c1187fcab1d88f3e670f996dfa94bf45dbc14cfe205e44e5e5a4c`,
`45b25d9a72d824c190bd93f2b1b2da22a8de21db8b09dba88a2d00860cb28847`,
`57568e320e8249b04aa1616e74d8d4133d990726bbe33faeef243117752ec99f`,
and
`c696aa92599f31e28fe5a1f9d077856844dc024fbb635af83c2af8a259144a1d`.
The reverse-dependency run completed 10 of 22 rows without finding a candidate
defect. Its six intentional Paradox-2 compatibility breaks are covered by the
prepared downstream heads; the other retained failures are reviewed unrelated
or environmental results. Commit
`a3689c29a667a1244c7cb008beb495b8ed550b88` repairs only replay validation of
the refill-wave bound and does not relabel the `b650735` execution. Corrected
completion/results/acceptance/waves/manifest/seal SHA-256 values are
`0ed2690ad40a2d5a907184cbb75859a9a3e70ea38b2c1b8120722d4aae63bb33`,
`53a64ba02a06fa4640eb065abd9bf04c961f6fdd2bdefb980f2a4d673c2d4000`,
`9bc0e18b3e1862f67da066c96c3003312cd545aa3e6174d0232bf94ef73b3b03`,
`2733911aef02cd305609b664169f397201e0c169aea2298fae7122694d665c23`,
`3f8a11be96975400421cb23cea0086cc82f99e4096ee9e73abd5fac59935e31c`,
and
`9b946c3cd55622aad967aad5c3563ba4dbdfd926038b674f8a410d1e2bff6cfc`.
The runner retained by the original execution still has the pre-repair bytes
and reproduces the known refill-wave validation error; replay requires the
corrected `a3689c2` verifier. This is an explicit harness-evidence boundary,
not package or artifact corruption. The containing compatibility coordinator
has status `failed` because the broad and reverse stages deliberately preserve
nonzero status for reviewed non-green rows; do not call the coordinator green.
Its completion/JSON-summary/TSV-summary SHA-256 values are
`354c55591cd0bec2600b0443acb3091773ce31a432812671c4e49e7decfb0840`,
`7beb0f6bfb19da5d1a169aab6a0b7ca50b9954ced362d2b38c4dccccee4032ec`,
and
`595dafad1cb8fc4c0229cb8da1a9823200bfd8c10af9092b0aea9f22af92ffcf`.
All seven mandatory documentation rows pass and 14 of 17 rows pass overall.
Results/manifest/seal SHA-256 values are
`12da725ef6e271f87627f483379285408927ea4641f5beba0959e5b6f024387f`,
`696ddc76ec7c17220bb0286fae0bfe7764b8fd79544356e60c6e7cf769e43d1b`,
and
`0e58a7a58088b48d0123dd7161a9104aba846fffb6a24e20a7cdabdd42c6b6b0`.

The final ten-package profile is frozen at
`refs/paradox-release/validation-tooling-20260801T222635Z`, commit
`ba51b7e056a5c84d2e2a7e02fc517617aeb235fd`, tree
`3effd430c0f76d22d20dbcebb3fe60ddb4d11c79`, and remains
package-facing-source identical to candidate `4e549f3`. Fresh Paradox-1 and
Paradox-2 preparations reproduce the exact dependency-library endpoint
`3828cdaf0c767d89fa30ea595845d3eab9695dfdb80bee909a5b3fabcc31919e`;
both ten-package overlays pass independent validation. Every one of the eight
prepared heads builds and checks with final `Status: OK` on both axes.
Paradox-1 completion/results/manifest/seal SHA-256 values are
`794bf1a58d92c27d8afe3a8a0706ab2f93f829fda9f813219470e1c12f6d713b`,
`434d22063a1eb1db58bb6fc93c8e2195858650c1306693dd66aa8e06a0b806c8`,
`f58b4ca0a91b8fb6b1900c1347bb6716a65f2f392e0932a376a859ce4379a4b5`,
and
`8389d22bf9070d2daa6807f85d89765a6fd91ef99adffa248dca22cfeb385944`.
Paradox-2 values are
`5384d6aa2b8c10381d684eb9c63deadecf863f128d95986d5393381aa96afcbb`,
`2e3f139454680633ae28ebe40f4ddfa05e554467221cb526756a84c944526b02`,
`e3c48c83614353718556f0b812b4a44742252820e6d3013a5023caee88aae9ad`,
and
`cd5b0b7bcbbd1edda8e0e5442e6b5909ae94ff6990b5a55c4635930b4c47863a`.

Final benchmark
`.local/benchmarks/release-candidate-4e549f3-final-benchmark-ba51b7e-r1`
passes its sealed 82-row policy: 79 rows pass, three are bounded marginal
reviews, and none fails. The marginal rows are
`design_transpose_plain` allocation, `shadow_values_live` timing, and
`collection_values_nested` allocation. Completion/manifest/seal SHA-256
values are
`a78db4858022b108a7f6f0f750057b8665a2b8b9a3a218f2b9ba4bb1845f9e88`,
`17d6c60d64005345cf8e00ebcf8adc2430dc71c0366f5e1b30cba7b6c000cdaa`,
and
`fc7853a81fab6be0fd0f5745aebdcd16687f16d503405baebe8db39d6239944f`.
All applicable local release gates are now complete.

That historical candidate's direct-child portability companion is
`refs/paradox-release/portability-harness-7b4440b`, commit
`7b4440b1b2e9fb75606da6f4bc8eb3cbf939bf6c`, tree
`850d8165877f4206294886fe046b995ae414bae0`. Its sole changed path is
`.github/workflows/r-cmd-check.yml`, SHA-256
`334699d5fbbaed85962ab079568d1cefb3772bbd468038772508ee66bdae0070`.
It is package-facing-source identical to candidate `4e549f3`; hosted dispatch
and publication are manual user actions.

The preceding immutable candidate is rejected diagnostic ref
`refs/paradox-release/candidate-20260801T034415Z`, commit
`fb2a37fc7d9b29d1998a8639a01e18130eaa4919`, tree
`86283a233c6de7d16c3d3bd20e682fa0ed5409c1`. Its `release-core` run passed
every non-runtime gate and all eight corresponding rows. `runtime-supported`
ran the full test suite with clean assertions on all seven exact minor-series
runtimes. R 3.6.3, 4.0.5, 4.1.3, and 4.2.3 then failed only because result
reconciliation found one list-ALTREP capability skip without its reviewed
ledger row; R 4.3.3, 4.4.3, and 4.5.2 passed. The affected stages stopped
before the old-R stress slice, final R-3.6 package check, and cross-version
serialization handoff. Those repairs were not accepted by this frozen run;
the later active `4e549f3` run supplies the complete every-minor acceptance
recorded above. Do not promote or transfer acceptance from `fb2a37f`.

In `.local/verify/runs/release-candidate-fb2a37f`, the four harness tasks, C23
under GCC 15.2 and Clang 22, the complete API-header matrix, all 33
differential cases, and the complete native-release lane passed.
`runtime-supported` built, installed, and ran the complete suite on R 3.6.3,
4.0.5, 4.1.3, 4.2.3, 4.3.3, 4.4.3, and 4.5.2; all assertions were clean.
Completion, JSON-summary, and TSV-summary SHA-256 values are
`9f84f7900b83671042f8ba7dc78016be1dd59f29c169e5abffb7a837a7cae8a8`,
`9ceb023f9d572f1273b1b456951b50bb07d0c8b8782098d588ed53902bcd7d6f`,
and
`9aadf3a9227779e1d4b04b5099362205b46e4091fbc5a36fcd2edf7bed1a8697`.
The missing derived result was `materialized and rejected inputs remain safe
under forced collection`. Reopened source puts the reviewed capability guard
at the start of that test and adds exactly four ledger rows, for R 3.6.3
through 4.2.3. The hidden fixture helper now errors instead of dynamically
skipping on an unsupported runtime, so reviewed leading guards are the sole
version-capability skip authority. This run is rejected diagnostic evidence,
package-facing source is reopened, and complete acceptance must be rerun.

The preceding immutable candidate was rejected diagnostic ref
`refs/paradox-release/candidate-20260801T014150Z`, commit
`6f28daed7596413d5c1227134b2bfa28a2ef7721`, tree
`1b283f19be4534ed30b86017e75eaa9426ec31e2`. It is
package-facing-source identical to `de1752f` and contains the recursive
test-support staging repair. Its `release-core` run passed every non-runtime
gate and all eight corresponding rows. `runtime-supported` reached and ran all
seven exact minor-series suites, and every recursive support-tree receipt
passed, but the suites exposed one old-R diagnostic-encoding defect and three
test-fixture/policy expectation families. Its focused repairs were exercised
by the later `fb2a37f` run, which is separately rejected for the reviewed
skip-row omission above.

In `.local/verify/runs/release-candidate-6f28dae`, the four harness tasks,
C23 under GCC 15.2 and Clang 22, the complete API-header matrix, all 33
differential cases, and the complete native-release lane passed.
`runtime-supported` failed only after R 3.6.3, 4.0.5, 4.1.3, 4.2.3, 4.3.3,
4.4.3, and 4.5.2 had each run the staged suite and passed the bracketing tree
receipt. Completion, JSON-summary, and TSV-summary SHA-256 values are
`9c1b797ae67324bf448f5c07227ee1a2264f406b61bc040dd783e54e5df3be5a`,
`521bf58a476b816920132b3e8432b4b8dfb79a080429bd567281baeb26a0cfe6`,
and
`e25224aeca5225f28c5277bb9d00f2be2674f5be43ecfa2b0dff6563d61d97da`.
The bounded failures were invalid-native-byte diagnostic rendering on old R,
portable no-`DATAPTR` ALTREP fixture construction, version-dependent terminal
callback counts, and the intended R 4.5 promise-inspection fail-closed policy.
The production encoding fix and the three fixture/expectation repairs were
focused development results at that point. This run remains rejected
diagnostic evidence and transfers no acceptance.

The preceding frozen package-facing ref was
`refs/paradox-release/candidate-20260801T000111Z`, commit
`de1752fe2085dc021b2e6936bcd3d5c0c809d9d4`, tree
`b91022dfcae1f1daeb886533620a234228f08803`. In
`.local/verify/runs/release-candidate-de1752f`, the four harness tasks, C23
under GCC 15.2 and Clang 22, the complete API-header matrix, all 33
differential cases, and the complete native-release lane passed.
`runtime-supported` failed with the same pre-test staging diagnostic on all
seven runtimes. Every stage had already built, installed, loaded, focused-
probed, and symbol-audited Paradox; no stage reached testthat, and the old-R
stress, final R-3.6 check, and closing receipts did not run. Completion,
JSON-summary, and TSV-summary SHA-256 values are
`69634511a79c0d8ab2b6b8dd92aa3b35012f58d420e1333cc4c1ba821cbc44e9`,
`6e6fa7f39e63a74890ecb9b2e062822b9add56a99b24ddc89986c057192a431c`,
and
`76d60a19e82fc0249e5f4bf8b3f8b81ff240181c04a8eec9f6e34676f1306e94`.
This run is historical diagnostic evidence and is not complete candidate
acceptance.

The repaired staging model executes only top-level `test*.R`/`test*.r`, as
every pinned testthat version does, and recursively stages every other
ordinary source member as support without flattening. The shared R-3.6-
compatible projector preserves bytes, modes, relative paths, empty
directories, and nested duplicate names while rejecting symbolic/special or
ambiguous members. A SHA-256 tree receipt brackets testthat execution. The
coordinator and independent verifier replay that receipt and separately
reconstruct the exact recursive projection; the old-R stress slice uses the
same mechanism plus its source-derived filter. Interactive structural,
adversarial, restrictive-umask, current-R, and exact-R-3.6 preflights passed.
The immutable `6f28dae` replay subsequently proved the staging mechanism and
receipts, but its runtime suites exposed the separate bounded failures above.

The preceding rejected ref is
`refs/paradox-release/candidate-20260731T225009Z`, commit
`1720d60c3bb8eefbc02d9b0455a9ee537b95a97a`, tree
`044a7fe8c58098f6739e5cef6e715c4bf30981d6`. In
`.local/verify/runs/release-candidate-1720d60`, the four harness tasks, C23
compatibility under GCC 15.2 and Clang 22, the complete API-header matrix, and
all 33 differential cases passed. `native-release` passed the strict full
suite, clean package check, and GCC analyzer, then failed only on the two
Clang-analyzer reports described above; `runtime-supported` therefore never
started. Completion, JSON-summary, and TSV-summary SHA-256 values are
`847829b78e6d5c58f360b10f0fef36c310514b3d2a42cb4ee5bc436a4ae65540`,
`63cdfae9d5b266ccb74963a07425669642d0546c2519fee868c418f3f132c135`,
and
`9f5abf153eb7d5f6f95d0fb34d98ca385d907fce39750a83f453a7e5aa29c841`.
This run is diagnostic only and transfers no candidate acceptance.

Clang's path model allowed the externally declared `R_NilValue` binding to
change across the `PROTECT()` which followed Condition admission. That invented
a path where an admission failure no longer compared equal to `R_NilValue` and
an unset `kind` was read. All three consumers now compare the returned snapshot
before the first intervening call and protect only a successful result. This is
safe because no allocation or callback lies between return and `PROTECT()`; it
adds no default enum store, retains the exact protection count, and compiles to
identical or better code. The first follow-up analyzer pass exposed three dead
`R_NilValue` initializers in `domain_row_admission.c`; the loop assigns all
three selected metadata values before its sole `break`, and its sole earlier
`continue` restarts the capture, so their removal is semantic- and
performance-neutral. The complete Clang analyzer preflight
`release-condition-admit-clang-20260731-r2` is green; its completion,
source-manifest, and analyzer-report-list SHA-256 values are
`d8fa00a349db9561f1e0e014e653f963afb474ab9136d1a0636359f2eb496811`,
`f2e055c67b833e380067d0eef6bec0429cb0b8ab5f42e631c8b9f9f6a2b2406c`,
and
`00ed8c7e5176591f023ceb7928cab3917949cb50fb2872e446561003f3cff595`.
The first complete static/focused attempt,
`release-static-focused-20260801-r1`, passed strict GCC and Clang, focused
tests, and both analyzers before cppcheck exposed five model-only diagnostics.
Its `unix64` standard-library model omitted `UINTPTR_MAX`, and its
compiler-neutral C99 model erased R's public `NORET`, selecting the deliberate
unsupported-width `#error` and four impossible post-`Rf_error()` NULL paths.
Package source was left unchanged. The harness now supplies those two exact
facts rather than suppressing diagnostics or broadly impersonating GCC. The
complete 39-source cppcheck harness replay
`release-cppcheck-model-20260801-r1` passes; its completion, source-manifest,
and cppcheck-log SHA-256 values are
`719792c414b3a51d0a7994aaf54a7352cc603dc299dac813b519ed40c8636fc2`,
`8e0329bca4243941d42d8d10fafaa727033907b52a02f8cd0c79a65212ed01e2`,
and
`c88c365634d0813b982fc0a97c550a5feb0a8e7a833e423d39712d2a96faaad7`.
The exact package/harness-source replacement preflight
`release-static-focused-20260801-r2` then passed all six selected modes:
strict GCC plus the focused suite, strict Clang probes, both analyzers,
exhaustive cppcheck, and the symbol/registration audit. Its completion,
source-manifest, GCC-analyzer, Clang-report-list, cppcheck-log, and
registration-log SHA-256 values are
`f45c566a308340c45475dd25682ba31085c5d4e9a2c986e894d4a288c39a2415`,
`222b0df99e5bec279d95bb7fab56e36533d2692767364be1be62084d688a618e`,
`c4ecb5d84e31537baf2fab335b445b2311ec23cb2a68df3bc232505691ecac33`,
`210b2dd82ad36618afd565499b1b780921e538787d3c13f320d9105e7508ac22`,
`779667b845361e3b31367871745c6c74af7026e0dafda8aa04a5b1de6c2b9391`,
and
`bebd79a1f7b3333b0db3a671cf518e8391a5f89c1d68cd69689639e676753e68`.
These are focused development results only; the next immutable candidate owns
every release gate.

The preceding rejected ref is
`refs/paradox-release/candidate-20260731T221636Z`, commit
`d892d94b11109fd2817b3f78db2127781cb35542`, tree
`6d93885457f42f57026a57d5a278bcd05dfde6ce`. In
`.local/verify/runs/release-candidate-d892d94`, seven tasks passed:
the four harness tasks, C23 compatibility under GCC 15.2 and Clang 22, the
complete API-header matrix, and all 33 differential cases. `native-release`
completed the strict full suite and clean package check, then stopped at the
GCC analyzer diagnostic above; `runtime-supported` therefore never started.
Completion, JSON-summary, and TSV-summary SHA-256 values are
`5f40ae2ac9c61992430f6fb3336427a6406476b269ae01264740252daad55a6f`,
`0f8bb02da2eb087f6044c4ebdad82cb227ca0b2a63e03b620dacb6f60d0d5221`,
and
`2e4c6f61b82f7b67c2897ab4eba45e911bdc722bef5343ea0dd5beb252327849`.
This run is diagnostic only and transfers no candidate acceptance.

The behavior-preserving workspace rewrite has a complete GCC-analyzer
development preflight at
`.local/checks/release-domain-workspace-analyzer-20260731-r2`. Its completion,
source-manifest, and analyzer-install-log SHA-256 values are
`5231952a3868b4a58d28084ba8b36aa4f35e99de925da4ac86b13761f94fb723`,
`320e7a7a320e844b1267b73492fe672650a7d49d537a6188a89501fb20d31f13`,
and
`36a72e83ef84dc4f152536290c231133c030a813c5d1712fbd19bb168144b3eb`.
The installed-package selection covering Domain construction, nested
admission, Domain kernels, ParamSet domains/quantiles, and TuneTokens passes
all 1,591 assertions with no skips. Independent strict R-3.6-header,
GCC-15-C23, and Clang-22-C23 compilation also passes. These are focused
development results only; the next immutable candidate owns every release
gate.

The preceding rejected ref is
`refs/paradox-release/candidate-20260731T215429Z`, commit
`41dc51dc4f6de4d92999a34cbd2cab6f17364db7`, tree
`85f2bf5e84a9df32143e1b9d3e404c74be831de9`. In
`.local/verify/runs/release-candidate-41dc51d`, seven tasks passed:
the four harness tasks, C23 compatibility under GCC 15.2 and Clang 22, the
complete API-header matrix, and all 33 differential cases. `native-release`
passed both full test executions and all functional check stages, then failed
release policy solely because `R CMD check --as-cran` ended with the one
code-analysis NOTE above; `runtime-supported` therefore never started.
Completion, JSON-summary, and TSV-summary SHA-256 values are
`c46f32dad37d5d6b2fb8dab1287433eb4bfc30ecbe911c449259052d957f167f`,
`23b1a0374a849d26fc96b79b4dc2b36ef96372bfed822270da4cc62d520d889a`,
and
`cf9bcef5fe4784fb85fc5297f2ee43e83776279abd51ede3181e6c04c119f96e`.
This run is diagnostic only and transfers no candidate acceptance.

The preceding rejected ref is
`refs/paradox-release/candidate-20260731T211009Z`, commit
`d6ad5c60af10887b07f406464b551a3007d1a9c5`, tree
`4083e33553d8a33efe1d4a0545150f90ecfb5519`. In
`.local/verify/runs/release-candidate-d6ad5c6`, the four harness tasks,
C23 compatibility under GCC 15.2 and Clang 22, the complete API-header matrix,
and all 33 differential cases passed. `native-release` passed both full test
executions and failed only when `R CMD check --as-cran` reached the
function-valued `to_tune()` example; `runtime-supported` therefore never
started. Completion, JSON-summary, and TSV-summary SHA-256 values are
`88f813e9afa670c872cbd9a45847f3317e1f2039a2e7c5df8070cf0400c6d63f`,
`3a60e34d549b9152cdba8a48039f7a0604e2ba20cf15ff68709c787fe50a745b`,
and
`e9a63e05f14c79408ad96b7cd0821332b767e8b34787893c509eaf83391041f3`.
The run is diagnostic only and transfers no candidate acceptance.

The narrow replacement delta has a strict mutable-worktree preflight at
`.local/checks/release-tune-repr-preflight-20260731-r1`: strict GCC and Clang
GNU C99 builds, all focused tests, and both native-probe inventories pass. The
complete `test_to_tune.R` file also passes against that strict-GCC install.
Completion and source-manifest SHA-256 values are
`31a267cb2ca84a632a84e20f154d144c8be5c5408c04ab95c4008a4bc89bb06f`
and
`8d5c6970427dd7c87aae8f978c7ed07c837076e0dd0b6fc434fdf1bb9194364b`.
This is development evidence only; the replacement immutable candidate still
owns every release gate.
The bounded indexed-root Domain admission compaction is governed by
[`domain-admission-receipt-compaction-plan.md`](domain-admission-receipt-compaction-plan.md).
Its implementation and focused correctness proof are complete; the plan's
first source-bound three-way timing attempt used
`refs/paradox-release/candidate-20260731T124039Z` at
`024e28f770f8dd7a8802b70a4a8e866ac2cb344f`. It passed all bulk Domain timing
and allocation gates but rejected direct one-row ratios of 1.213x and 1.141x
against `e923c1a`. That ref is diagnostic evidence, not the release candidate.

Replacement timing candidate
`refs/paradox-release/candidate-20260731T150816Z`, commit
`a153faeeed8735c4aeeba512a70fb71b11235469`, tree
`2fe241b90b0e86381a6f884fd29d3a80baab6b7b`, passed the corrected comparison
under
`.local/perf/domain-indexed-root-final-threeway-candidate-a153fae-repaired-r5/results`.
All eight gates pass. Completion, gate, round, and summary SHA-256 values are
`379d4c1839c2d05f8c7c8c9d867f492d68355ef3fc22eae46fdf36a389a417d7`,
`e87450b0ab084a0f43867daeb6aee7473eea888b952af9a306c894fa7f89dce2`,
`0cfb24b08727eb930ee34f42384242abaece108181e70e66bbc1bddea7c60d8c`,
and
`ed1d3d6efc8f311d80e8371e0d055aebcdc87f3f5c19d6d1950ab3386672cd43`.
This closes the bounded comparison; do not rerun it for unrelated cleanup.

The candidate's `release-candidate-a153fae` coordinator passed five tasks,
failed three, and dependency-blocked the every-minor runtime task before it
started. The four harness rows and API-header matrix passed. C23 stopped before
compilation because the validator expected an obsolete micromamba inventory
header. Differential completed all 33 cases with zero unexpected differences
but seven stale candidate hashes. Strict GCC completed the package suite and
exposed six errors: one stale expected diagnostic, one genuine formal-S4 leaf
misclassification, and four authentic populated Paradox-1 keyed
transformation tables whose historical spelling omitted `row.names`. The
completion, JSON summary, and TSV summary SHA-256 values are respectively
`83140b9737809c8b8db6457160938bdc5a39b6db4531fef42962508b61e3e165`,
`e73d372b7533782ba66f922948c7acb4764aa115cafe02c9a64973a531b5269f`,
and
`f912757fe598e4e5fa269e6511f93969b20ebf7aee9d22c9fbd3881626755737`.
The run is diagnostic only, supplies no donor, and accepts no package bytes.

The final mutable-worktree convergence preflight is
`.local/checks/release-fixes-preflight-20260731-r5`. Both strict GCC and Clang
GNU C99 installations and all 88 focused source files pass after the
formal-S4 marker, coherent migration snapshot, nested receipt-lifetime, and
diagnostic repairs. Its completion and source-manifest SHA-256 values are
`0f306c6e698e39e2720774ed02fab01171919c6e62a4c0d046fdf6ee4c38ff67`
and
`4aa0bb3b7f3d5c57176b6e56145aad9f8d97cf0a866a5ae5f0cbd724a872fdf6`.
This is development evidence only. The immutable candidate and complete
release gates below remain mandatory.

Section 11.5 records the implemented recovery and exact focused evidence.  A
single five-cell metadata capture now feeds generation-local names/column,
row-count, class, self-reference, and representation receipts; the shape path
sizes from ID and never observes callback-capable row names; and pointer-first
built-in dispatch removes the profiled repeated work without weakening the
three-generation proof. Review also fixed early ALTREP row-name observation,
duplicated typed-class resolution, and malformed factor-level type admission.
A genuine raw cyclic attribute spine cannot be constructed by the supported
public APIs without the forbidden `SET_ATTRIB`; it remains at the reviewed
hard-bound/static-proof boundary rather than creating a package policy
exception.

The exact source/test diff against plan commit `aa0bff7` has SHA-256
`8413a97b358ba9a13a45e1137559dd1a4d4d7477a5562d5180f026d82701ac98`.
The current-R focused test, native-probe TSV, and public-API audit hashes are
`30125069780d5c650055436a9a168092cbaf4f4bd8428db41d1705716861e78b`,
`63c1bcf383aefec6ae729cee086ef775651e22865f5c6f2d654f1c4e7b5f1366`,
and
`2a68d9c8376847945c88837caa2342ac184fb8f0b180f64d1e05f53f57a7febc`.
The corrected actual-R-3.6.3 final install and focused-test logs are
`9b905c9f57f46009f289be8dc5d8cd6f402c0e11c4874283635fe52cae14e3b9`
and
`11a3146ba597a593faf31845e1e59314d962e0423db7ebea34e5414851f46589`;
the tests ended `DONE` with only the three expected pre-R-4.3 list-ALTREP
fixture skips. Strict GNU C99 GCC/Clang logs are
`584609789585a6f8d49141b742938f3bd12cc7d7203f23b9a26f87b8f60c5e3a`
and
`9b81d1accc20458fdaa079d714abb8a2ea60c5865aefacda9ec2ca174955bf01`.

Callgrind reports 15,520.245 instructions per direct double check and
11,355.074 per no-op utility sanitize: 0.706x and 0.604x the rejected
candidate, and 0.981x and 0.870x the exact `e923c1a` baseline.  The bulk total
is 0.99154x exact `aa0bff7` and its C-entry ratio is 0.98914x.  Exact report
hashes and all package-file hashes are in section 11.5 of the Domain plan.
These instruction results do not substitute for the corrected immutable
three-way wall-time comparison or any full release gate.

The exact focused closure snapshot is
`.local/checks/integrated-selectors-r9-20260731T122010Z`, with source-manifest
SHA-256
`0a000dfb692332aed5eacd80119b9ae3fdf037e21a31fa7bf1cf60956584db0a`.
Independent review closed three last issues: Condition operands undergo one
hard-bounded structural re-admission after callback-capable `Length`; the
Domain empty-special names-presence workspace is initialized for every
interpreted row; and the rejected broad structural-ALTREP-names exception was
replaced by materializing only package-produced categorical names and emitting
compact ordinary facade row names. The directly relevant package-source
SHA-256 values are:

- `src/r_utils.c`:
  `f9b81c1d3aca0f99bae5bfc3625ba252eb513eb177c2e1b6dbf8d760110104fb`;
- `src/domain_row_admission.c`:
  `68233a976348f6b09ff42f5157fc780a3451fb52c797a91c85cbf82cc71e38c5`;
- `src/builtin_condition.c`:
  `621ce8498691a7ff60ba39e8cff617024664738c505a295cfa310cfd854aceff`;
- `R/ParamFct.R`:
  `4ee3b9179ff78880969acedcaafefb2f0eb8356c87d8a35b3d5ee590e16a3d53`;
  and
- `R/ParamSet.R`:
  `e676006d826fc97089d56ff5852fa369b79de4f9250b44cb7bfba7b3a66d940d`.

The retained current-R `Condition`, `native-design-transpose`,
`native-domain-construction`, and `native-public-accessor-ownership` logs plus
the actual-R-3.6.3 accessor-ownership log ended `DONE` with exit zero. Their
SHA-256 values are
`7e494791778fad9946d60883cf0d514b403dfd11b312c80af2bd3c550fd2a27d`,
`eb1940f1604ef0174515ed11db7ad6584f7b78ecb1c3e2389178da29221e91e0`,
`033caeaef2f31c11d05f2bcaefb584d47edc60bcfe3cf2737d61a9a980d24df3`,
`9c97379a6cb472533ee656ceb683e83f850c7c15f9ced6e507bb38f086e4d3d6`,
and
`c95f6a004903c3bd1d4d0bdc70847254fcd2a8410438632ee770c5feec791b2f`.
The C/R implementation is byte-identical to r8 manifest
`6ed37e6c07ca17d8f487a949cfc2e4cf510eaded448e89fc1b11c7750a114634`,
which owns the retained strict GCC/Clang GNU99 and current/R-3.6 package
installation proof. This is focused development evidence only, not candidate
acceptance.

An earlier historical candidate is
`refs/paradox-release/candidate-20260727T152133Z`, commit
`dbbdcc156cb52793e84e8767f0ce84b6ecbb85ea`, tree
`b60b75e3923cdcb49f1ef2fb0f9b386d5cac291d`. It contains the complete
contract-first implementation, R 3.6/C99 compatibility work, and the managed
active-path graph carrier with ordinary and instrumented regressions.

The exact `release-candidate-dbbdcc1` run passed all eight `release-core`
tasks: controller/native/runtime/validation harnesses, differential, API
headers, native release, and the supported R 3.6.3/4.0.5/4.3.3/4.5.2 runtime
matrix. The coordinator completion, JSON summary, and TSV summary SHA-256
values are respectively
`511da5ccf2ef4e779db97bc6e79ab458f861148b4841625132e613a2b285d090`,
`fe9b41d7552d73e2d342cfa1f3c666cb672508d4d29aef6d3c7dc063ee380e44`,
and
`507160680c2a861ece452f8a23f12caceb590897afec00a38a5757229709b522`.

The native child
`release-candidate-dbbdcc1-native-release-a001` is not an eligible memory
source donor even though its package work passed. Its source manifest retained
the original worktree modes while its copied source tree retained
umask-filtered modes. The first combined-memory attempt,
`release-candidate-dbbdcc1-memory`, then failed source-proof preflight because
the relocated validator lacked its trusted
`create-offline-check-repository.R` sibling. It stopped before loading the
candidate or starting GCT, Valgrind, or rchk, so it is neither a package failure
nor memory evidence.

The six-file harness repair makes snapshot and replay modes exact and
umask-independent, retains/hashes the offline-repository helper, validates the
same exact relocated bundle independently, and tests the generic sibling-set
relationship. The old run directories remain immutable. Replacement
coordinator `release-candidate-dbbdcc1-native-replay-r2` passed all four
selected rows; child
`release-candidate-dbbdcc1-native-replay-r2-native-release-a001` passed the
full native lane and direct independent source-run validation. The repair is
confined to package-excluded harness paths, and committed repair `50a8593` is
package-facing-source identical to the candidate. Generated package archives
are not claimed byte-identical because R injects nondeterministic metadata and
vignette output.

The replacement coordinator's completion, JSON summary, and TSV summary
SHA-256 values are respectively
`2b4781b276583903f929f3b659037ce1ec228dbbe3755ca7ca2c29a2d776ba01`,
`9a279062fa8f2d99ff97561b6530dd3944614d04fc133e953b772aee64f5cb80`,
and
`92eb669197c8cd82a43f2424143efe7f3967213a3a1da2006bcdc691ce519bd2`.

The first rchk discovery attempt failed resource preflight without starting an
analyzer. After memory was released,
`release-candidate-dbbdcc1-rchk-discovery-r2` admitted the full 20-GiB
analyzer allowance plus 16-GiB host reserve. All three analyzers exited zero;
the retained run failed only at the intended comparison with the historical
policy. Bcheck analyzed 951 functions and 50,395 states, producing 83 Function
blocks, 239 UP, and 17 PB. Its raw and semantic SHA-256 values are
`ddbad6140d74e96422bc4f77942f9303af9e51706005f9f95d36b3d677b011d8`
and
`9135cbe02ddbd69019d7901d09bd5a7286fd24b74260e1864be0c92aeceef1d9`.
Maacheck is byte-empty; fficheck reports the exact 82 registered functions and
one registration call, SHA-256
`448c7d8dae05fab9b43570ed169a4472de8ca6ca40836061642c4203c3391882`.

Every changed block was source-reviewed and no native defect was found. The
four new PB diagnostics are confined to bcheck's branch-dependent model of the
balanced list-column path in `build_dependent_grid()`. A harness-only GCT probe
now combines dependency expansion, a list-valued fixed special value, and an
inactive row. The independently generated and validated policy, block table,
and unchanged rationale catalog SHA-256 values are respectively
`eb1e2e9d89a27b9a43a7be87606c22b71fef636284c5c570c5f795aa2345833f`,
`d0ba3cbc6d2b536b157939fc3c6cbe2d9fcf5b2ed497a088234ee345dddf2fad`,
and
`c9e94a9f49b5570838df94fba7f45ef870999b78d03b7b4824412dc34645d8a4`.

The finalized repair passed Bash syntax and R parse checks and the full
activated validation-hardening suite in about 226 seconds. Its adversarial
fixtures cover exact `0664`/`0775` initial and replayed modes under umask
`0077`, missing/tampered relocated helpers, and equality of all 21 copied,
hashed, and independently validated harness inputs. Actual R 3.6.3 and current
R probes also confirmed exact mode restoration with `use_umask = FALSE`.

The exact combined-memory replacement
`release-candidate-dbbdcc1-memory-r3` passed GCT, Valgrind, and the reviewed
rchk policy. Independent offline validation selected all three modes with
validator SHA-256
`0d922790f73550d6a70bebd83008ce099a86f77fe0af18758f2332e97374b3b6`.
The completion-content and result SHA-256 values are respectively
`b903b0ba50f50d68704e947a725b47f0569bf395e5304f6d86143c580c560e44`
and
`d260a45c237654509a21e5579e392eb6278afec0c89c43863f30819cbbd9409e`.
Its rchk policy-difference diagnostics are byte-empty.

The first prepared compatibility coordinator,
`release-candidate-dbbdcc1-prepared-compat-91ca588-r1`, retained four passing
cheap harness rows, two failed cheap harness rows, and six dependency-blocked
rows; no real compatibility task started. It is immutable diagnostic evidence,
not a candidate failure. The reverse fixture exposed a procps-ng 3.3.17
parsing hazard: an unseparated negative process-group operand can become
another signal option and target process group zero. The wrapper now uses `--`
before every negative group operand. Removing the UTF-8 declaration from the
ASCII-only `Authors@R` fixture let that cheap lane proceed, but the subsequent
real checks proved the root cause was production `env -i` discarding
`C.UTF-8`. The final correction restores the representative UTF-8 declaration
and explicitly pins `LC_ALL=C.UTF-8`, `LANG=C.UTF-8`, `LANGUAGE=C`, and
`TZ=UTC` in fixture and production build/check rows. The strict real-check
status classifier remains unchanged. Every published overlay and failed stage
remains immutable; corrected tooling always uses a fresh owner/coordinator.

The immutable `4c4cb53` replacement coordinator completed all unblocked work.
Its six harness rows, overlay, and 17-workload documentation gate passed. The
focused five-package lane built and checked every package with process status
zero, then correctly rejected the shared locale WARNING. Reverse preflight
stopped before package work because its valid coordinator-assigned finite
`operator_max_jobs` met a stale reverse-only prohibition. The broad corpus
completed 28 one-row waves: 17 pass, the new `mlr3tuning` and
`mlr3pipelines` failures are dormant-value expectation drift, and the other
nine remain scoped optional-system, dependency, upstream-fixture, or timeout
failures. Its completion/rows/manifest/seal SHA-256 values are
`7e4cb24e4e5141c5ae2dcfa36f67c406972ac75b60a98d5b14d17a7fa5a12fd0` /
`9c7cbcecb8faa7419551a0d84d9f6dd138c2564c719f951ceaba093ebea0cee0` /
`efb8f8a3cfa801c52ce17f9fa78ef2c2a7b462d05f9c091efaf0dd88a89b8d26` /
`97cc58b471812e74bd89206004677dd31be05017d478fff7d7592062884a4785`.
The finite-cap/locale/aggregate-weight correction then passed the direct
resource scheduler fixture, complete reverse self-test, downstream
bridge/profile fixture, all 66 controller tests, and the verification-economy
contract. Broad gates remain deliberately pending until the downstream
dormant-value heads and exact manifests are refreshed.

Normative contract: [`contract-first-2.0.0.md`](contract-first-2.0.0.md).
Implementation map: [`architecture.md`](architecture.md). Compatibility and
migration policy: [`compatibility.md`](compatibility.md). Validation sequencing:
[`validation.md`](validation.md).

## Decisions frozen for the first public release

- Version is 2.0.0; R >= 3.6; portable C99; ordinary current-R installation
  selects C17-or-earlier through `USE_C17`; an explicit GCC >= 15 and
  recent-Clang C23 gate supplies additive forward-compatibility evidence;
  data.table >= 1.18.4.
- R C API use is public except for the exact centralized, versioned
  raw-attribute, coherent old-R closure-snapshot, and non-forcing
  stored-binding/promise compatibility
  entries described below. Raw attribute selection uses `R_mapAttrib()` on
  R >= 4.6 and one ledgered `ATTRIB` traversal on R 3.6--4.5 without
  evaluating R or data.table code. Before R 4.5, exact ledgered `FORMALS`,
  `R_ClosureExpr`, and `CLOENV` accessors capture one allocation-free closure
  generation for callback admission and recursive migration; all three
  exceptions compile out at R 4.5. Directly reached bytecode alone uses the
  cold, non-executing public `as.function.default()`/`body()` bridge on old R.
  R 3.6--4.1 use `base::exists(..., inherits = FALSE)` only for cold optional
  existence queries; required ordinary-frame binding snapshots stay
  allocation-free, and the terminal optional receipt scan uses exact old-only
  `R_HasFancyBindings()` to fail closed for a fancy frame.
  On newer R the authenticated ordinary-frame path is likewise
  allocation-free. Callers retain one conservative rooting proof across the
  supported API branches because hostile class metadata can allocate during
  facade admission.
  The facade rejects the recognized `UserDefinedDatabase` class before all
  binding APIs; callback-backed object tables are not ParamSet/R6 shells, and
  old `R_HasFancyBindings()` is valid only for ordinary frame layouts.
  The compatibility facade uses exported `Rf_findVarInFrame` on R 3.6--4.5 to
  retrieve a stored frame cell. R 3.6--4.4 may inspect a returned `PROMSXP`
  through the header-declared/exported `R_PromiseExpr`, `PRENV`, and `PRVALUE`.
  R 4.5 compiled-code policy classifies those accessors as non-API, so recursive
  migration fails closed on a reached promise and requests R 4.0--4.4 or
  R >= 4.6. Ordinary factory callback frames can retain such formal promises
  even when the argument was forced or unused. Every object-retained generated
  callback frame stores direct captured values, avoiding that boundary inside
  current Paradox objects.
  R >= 4.6 instead uses only its experimental binding/delayed-binding/dots
  APIs. An R >= 4.5 DSO excludes all three detached-promise accessors. On
  R >= 4.6, a `PROMSXP` reached outside a binding/dots cell is opaque.
  Public `R_getVar` is also excluded before R 4.6 because, without the binding
  classifier introduced there, it can force a delayed binding. Current-R code
  has three reviewed call sites, each after `R_GetBindingType` proves a direct
  or already-forced value; the DSO has one undefined-symbol inventory row.
  Simple Domain rendering is not a second old-R implementation: one native
  renderer receives `scipen` from public `base::getOption()` on R 3.6--4.4 and
  from documented `Rf_GetOption1` on R >= 4.5. Exact DSO inventories forbid
  that symbol on the former runtimes and require it once on the latter.
  An R-level `substitute()` workaround is non-forcing but unsound for
  simultaneous receipt and recursive object-graph scans because it returns an
  expression, not a binding-kind/generation receipt.
  The native direct-binding projection distinguishes realized language/symbol
  values from delayed promises carrying the same expression types without
  evaluating either.
  Each exceptional symbol, version, source count/path, and rationale must be
  recorded in `environment/r-api-exceptions.tsv` and pass raw-token, DSO,
  pinned-header, and real-runtime audits before freeze. None is a CRAN allowlist
  or broader internal-API permission.
  The R 4.5.2 runtime stage additionally retains a manifest-bound zero-issue
  receipt from that runtime's own `tools:::check_compiled_code()` over the
  installed package.
  R 3.6 has no accessor for an active-binding function. Direct and recursive
  legacy ParamSet-family migration therefore fail closed when its inspection is
  required and ask for R >= 4.0; because Paradox-1 ParamSet-family R6 shells use
  active bindings, practical migration of those objects requires R >= 4.0.
  Exact built-in current Paradox-2 shells retain recursive traversal through
  their authenticated capsule. Package active facades are opaque on R 3.6:
  unsupported in-place replacement cannot be distinguished when the exact
  shell receipts remain unchanged, and its closure is not traversed or invoked.
  Additive shells and modifications that fail exact authentication instead
  fail closed. Current
  operations, idempotent current-object conversion, and standalone legacy
  Domain/Condition conversion remain supported on R 3.6.
- Public old-header adapters for raw/complex setters and default
  `identical()` flags are inline. Collection parameter reads pass the admitted,
  rooted core directly to the shared loader, eliminating both a temporary
  environment allocation and the need for post-3.6 `R_NewEnv()`.
- R 3.6 cannot construct VECSXP ALTREP because R exposes that facility only
  from R 4.3. Its runtime row therefore version-gates only the adversarial
  list-ALTREP fixture; atomic ALTREP tests still run and the corresponding
  production list branch is vacuous.
- One opaque v1 capsule and BASE/COLLECTION/SHADOW node graph are the current
  state model.
- The public `assert_values` flag is the sole stateful R-shell policy outside
  that model. It selects checked versus unchecked native storage and remains
  clone/serialization/equality-visible without changing the eleven-field capsule.
- Capsule tables are plain data.frames; data.table is outward-only.
- Domain and Condition kinds are closed; ParamUty custom checking remains.
- Canonical built-in Domain-row semantics have one native admission owner shared
  by constructor final-state validation, ParamSet construction, and
  ObjectTuneToken Domain admission. Boundary-specific outward table/class checks
  do not duplicate row semantics.
- Every nonempty and typed-zero public Domain operation still proves the exact
  complete sixteen-column structure; the canonical zero-column empty Domain
  has its exact dedicated validator. The identity spine is always interpreted.
  Each operation declares a semantic mask over bounds, levels, special values,
  cargo, tags, and transformation, and
  `paradox_domain_interpretation_closure()` alone expands rule dependencies.
  `domain_check()` requests every rule. Irrelevant rules are skipped whole,
  not duplicated in operation-specific validators.
- A typed zero-row `domain_qunif()` validates `x` before returning the kind's
  mapped empty vector: numeric for Dbl, integer for Int, character for Fct, and
  logical for Lgl. ParamUty retains its undefined-mapping error at zero rows.
- Standalone `condition_test()` uses the registered closed comparator for
  `NULL` or plain logical/integer/double/character vectors with names only;
  stable ALTREP operands materialize once and classed/attributed operands do
  not dispatch through `Ops` or `%in%`.
- Built-in Condition RHS values are attribute-free, non-missing vectors of the
  same four kinds. `CondEqual` has one element and `CondAnyOf` is non-empty and
  unique. Public admission roots and materializes the RHS once; strict capsule
  validation accepts only that ordinary snapshot.
- Additive-only ParamSet-family subclassing is supported; core ParamSet
  overrides/private state are not. The documented Sampler subclass API remains.
- ParamSetShadow belongs to Paradox and replaces miesmuschel's private-layout
  implementation on the Paradox-2 branch.
- `ParamSetCollection$add()` is an atomic native replacement transaction. It
  follows Shadow origins, rejects corruption and existing/proposed cycles
  before commit, and generation-checks the complete current/child graphs.
- Native capsule graph-path admission roots every active shell and exact
  selected core generation in one managed carrier. Raw frame storage is
  scratch only; carrier slots are cleared on pop and grown before an allocating
  frame/child transition. This protects ancestor edges across old-R optional
  binding evaluation and finalizer-driven `.core` replacement without a
  separate old-R implementation.
- Shadow live refresh is native and generation-based. Its sole non-payload
  attribute is an exact derived origin-graph signature; `.sets[[1L]]` is the
  only origin authority and fixed factories are not cached as state. The
  signature is package-rebuildable cache data but mandatory and exact on every
  current SHADOW core.
- Checked value assignment is one graph-wide native transaction over ultimate
  BASE targets, including collections and shadows. It deduplicates shared
  targets, validates once, preserves nested callback writes, and commits all
  replacements atomically without a second R or child-store pass. ParamSet
  Object-token inputs add rooted generation receipts and one final allocation-
  free candidate reauthentication immediately before commit.
- Standalone Domain checks and ParamSet scalar/table checks share one
  package-owned C value classifier and failure-only formatter. Ordinary
  missingness, type/shape, integerish, bounds, and factor-membership failures
  use informative checkmate-style categories and established fragments.
  Successful validation does not construct diagnostics; the native validation
  path neither calls checkmate nor repeats validation in R. Byte-identical
  reproduction of every checkmate quirk, `conditionCall()`, or unsupported
  exotic-object behavior is outside the contract.
- `check_dependencies()` reuses the native check graph/point/dependency kernel,
  accepts only an ordinary uniquely named base list, validates unknown IDs even
  without dependency rows, skips TuneToken edges, and returns the first
  diagnostic rather than reproducing R/pmap multi-error collapse.
- `test_constraint()` and `test_constraint_dt()` reuse the native check graph,
  point admission, and constraint kernel. A validating table call admits every
  row before any constraint callback and then evaluates one immutable
  constraint snapshot once per row; ParamUty custom checks may run during
  Domain admission, and reentrant mutation affects only later public operations.
- Tag access/mutation, dependency snapshot/access/mutation/append, and BASE
  callback replacement are native capsule operations. Bulk dependency
  replacement is a callback-free structural snapshot and preserves predicates
  made infeasible by parent-Domain narrowing. `$add_dep()` remains the strict
  authoring operation: dependency feasibility uses the shared check kernel and
  generation-checks callback reentry; Shadow append routes natively only within
  the fixed visible schema.
- `$has_deps` is one registered scalar reader. BASE validates its canonical
  dependency table, SHADOW performs one live refresh before validating its
  table, and COLLECTION admits the complete graph before reading the root
  subtree count. It never constructs a detached dependency/data.table facade
  or uses a cached or reduced-integrity graph path.
- A BASE-origin Shadow constraint closure contains exactly a callback and
  hidden-values plan. Its native evaluator performs the hidden-first merge
  without S3 dispatch, preserves leaf identity, calls once, and admits one
  non-missing logical result; collection origins stay on the collection native
  evaluator family.
- Current objects serialize normally. `upgrade_paradox_object()` remains the
  pure single-object converter, including standalone Domain/Condition
  normalization. `upgrade_paradox_object_graph()` iteratively discovers a
  containing graph and transplants admitted legacy ParamSet-family shells in
  place after complete preflight. It traverses ordinary containers,
  attributes/S4 slots, local environments, closure/bytecode structure, active
  binding functions, and promises without forcing or invoking serialized
  behavior; global/search/package/namespace infrastructure and generic
  external-pointer/weak-reference internals are boundaries, while authenticated
  Paradox core payloads remain traversable. Commit is post-order and monotonic,
  with `.__enclos_env__` as each shell's last completion point. Direct native
  binding classification distinguishes realized language/symbol values from
  promises without forcing either. Current shell admission uses one ordinary
  additive BASE/COLLECTION/SHADOW suffix classifier, exact `assert_values`, and
  canonical-core agreement. Read-only Shadow preflight retains separate
  source-generation and authoritative semantic-preview cores and constructs
  callback detachment from the admitted graph. Every prepared/current root is
  jointly validated before the first transplant; after each child transplant,
  its prepared parent is identity-rebased. All already-current identity roots
  plus that newly rebased prepared root are jointly validated before the
  parent changes; unrebased parents remain offside templates until their turn.
  The current identity-root set is checked again after the transplanted
  original joins it. A catastrophic
  partial binding wave retains the old authoritative enclosure and remains
  authenticated for retry; completed nodes are valid current objects. Current
  shells are preflight candidates as well as traversal carriers, so a corrupt
  current capsule anywhere in the selected graph aborts before any legacy
  mutation. Pending finalizers from unrelated user objects are explicitly
  outside this atomicity promise: when a transplant occurs, post-transplant
  joint capsule scans plus one final allocation-free complete public-binding
  receipt detect a selected-root mutation inside the R binding wave, but do not
  roll back a completed transplant or promise retry of the externally corrupted
  graph. An all-current graph has no binding wave and returns after joint
  capsule validation, including on R 3.6.
- Current R6 stubs call versioned namespace targets directly. Historical
  unversioned targets are cold first-use gateways: default error, or silent
  migration when `options(paradox.legacy_object_action = "upgrade")` is set.
  Direct forwarding uses one native rooted context: it selects the
  defining-family enclosure through the authenticated additive superclass
  chain, requires exact `assert_values` and a canonical matching core, and
  ignores the serialized stub's `private`/`super` promises rather than
  replaying or rereading a top enclosure slice.
  Historical Sampler1D private stub formals are part of the same serialized
  boundary: the cold `Sampler1DRfun$.sample` (including Normal) and
  `Sampler1DCateg$.sample` targets bypass the narrower Paradox-1
  `as_dt_col`/`sample_truncated` stubs and invoke versioned lower-level targets
  directly. Current versioned sampler targets remain gateway-free.
  The exact owner registry supports bbotk's additive legacy `Codomain` and
  miesmuschel's single-origin current-Shadow replacement/retired fields without
  S3 dispatch or serialized hook functions. Additive dependencies are empty,
  replacement dependencies are exactly `origin`, and owner R6 finalizers are
  rejected; unknown subclasses fail closed. Complete-session discovery now
  precedes owner construction, and focused adversarial preflight rejects a
  rebuilder result that shares any public, private, or enclosure environment
  with an original/current session node or any distinct prepared node before
  changing that result; valid shared origins and current identity reuse remain
  accepted.
  Built-in and owner method provenance is checked against the exact currently
  loaded namespace environments; namespace names alone are not authority.
- Stable/base ALTREP support is materialize-once in admitted semantic atomic
  positions. Configuration/search-space/trafo and ParamSet-`params` lists,
  internal table/row/Domain/Condition/token/capsule shells, Domain cargo/
  interpreted cargo entries, dimnames, class/name vectors, and other list
  metadata remain ordinary non-ALTREP/non-S4. The six public-table ingresses
  use one suffix-aware, allowed-attribute classifier. An ordinary well-formed
  class vector may have leading additive classes before its terminal
  `"data.frame"` or `c("data.table", "data.frame")` suffix. Those leading
  classes never dispatch. The classifier does not copy or materialize an
  ordinary shell merely to remove the prefix, and semantic snapshots ignore it;
  an already-required ALTREP snapshot installs the canonical suffix. Malformed,
  reversed, non-suffix,
  reserved-label, and duplicate class vectors reject. Names/classes and
  admitted data.table cache carriers are ordinary; caches are discarded. Raw row names
  are attribute-free, nonobject, non-S4 integer/character vectors: ordinary
  compact `+/-n` forms decode to their count, while stable row-name ALTREP pays
  one Length and no Elt. Row-consuming operations compare this count with their
  columns; direct `trafo` and a no-edge Design dependency plan do not add
  column observations for an unused dimension. A zero-column data.frame may
  omit names and retains its row count in Design transpose. Admitted top-level
  VECSXP ALTREP snapshots own names/class before callback-capable observation,
  use one Length/one Elt per column, and may retain stable semantic ALTREP
  columns. Base R's lazy attribute-copy duplicate is the common motivating
  case. Direct
  checked/unchecked `$values <-` rejects an outer ALTREP before observation and
  natively canonicalizes the Paradox-1 empty spellings (`NULL`, an ordinary
  attribute-free zero-length atomic/expression vector, or an accepted empty
  list container) to a named list; only
  `set_values(.values=)` has an operation-specific outer-list snapshot.
  Hostile state-changing custom ALTREP across prior R-side representation capture has
  no exact semantic/printed-representation guarantee. Paradox must neither
  replay nor itself cause a crash or memory corruption.
- Base `extra_trafo` results may remain unnamed for public and TuneToken
  compatibility; collection child results require complete unique names for
  namespace translation. Transformation results and non-table inputs have
  ordinary non-ALTREP/non-S4 shells. A documented data-frame input may use the
  suffix-classified top-level ALTREP table boundary above, while admitted atomic leaves and
  columns may be stable ALTREP. Both use the single native transformation engine.
- The unreachable namespace-level R `transpose()` implementation and unused
  `col_to_nl()`/`rbindlist_proto()` table helpers are deleted. Known consumers
  call the public `Design$transpose()` method; the removed internals were neither
  exported nor used by the maintained/downstream corpus.
- Live collection callback bindings, detached subset/flatten callback
  factories, and SHADOW adapters over COLLECTION origins all use that same
  native evaluator family. Their R closures retain only exact validated
  owner/mapping plans and contain no parallel callback selection/translation
  engine. Retained/untransformed inputs remain in input order, followed by
  changed child outputs in callback-plan order; omissions remove owned inputs.
- `ParamSet$subset()` has one additive final `keep_trafo = TRUE` argument,
  shared by COLLECTION and SHADOW. Setting it to `FALSE` strips both selected
  per-parameter transformations and `extra_trafo` in the native subset
  transaction while leaving `keep_constraint` independent. This public API
  replaces mlr3mbo's private Domain-table mutation; malformed Domains are not
  admitted for compatibility. Subset flags are exact attribute-free logical
  scalars, and COLLECTION callback detachment follows the admitted result
  without applying R generics to the original controls.
- Exactly two narrow cold R semantic-orchestration families remain, and neither
  is a fallback. The first contains the three internal-tuning operations—
  aggregation, disabling, and internal search-space conversion—as single R
  implementations over one captured cargo/translation/Domain/owner-value
  snapshot and commits through native mutation.
  After native flattening, the same cold family may rebind documented `cargo`
  closures and replace that one column in the detached BASE result. It is the
  first narrow exception to thin wrappers, not a second graph/check/value/
  callback-selection engine. The second is exact-TuneToken `$search_space()`
  conversion: it consumes one rooted native snapshot, switches only over the
  package's built-in token kinds, and solely owns callback-dependent
  one-dimensional output compatibility and outward search-space construction.
  It has no S3 extension or competing native/R conversion path. One-way legacy
  migration is outside this current-operation count: it authenticates a
  retired schema and orchestrates current native construction/validation plus
  R6 shell transplant, never an alternate current semantic engine.
- Ordinary non-ALTREP S3-classed named value-list containers are admitted with
  the outer class discarded; scalar Domain argument names are likewise
  representation-only. Direct checked/unchecked assignment rejects an outer
  ALTREP before observation and canonicalizes empty input in native code.
  Neither is an extension/dispatch mechanism.
- TuneTokens have one native exact-shape snapshot boundary: exact `{content, call}`
  names, five built-in class vectors, exact Full/Range/Internal content, and an
  admitted bounded value-producing built-in Domain or exact BASE
  `c("ParamSet", "R6")` shell/core for
  Object content. An unbounded `ParamUty` Domain rejects; bounded typed Domain
  coverage retains opaque leaves without treating ParamUty itself as a range.
  COLLECTION, SHADOW, and additive subclasses reject. Exact
  creator provenance is not inferred: a shell alias retaining genuine BASE
  private/core linkage may pass safely because C never calls alias methods.
  Scalar names are normalized away. Subclasses, extra/reordered metadata,
  S4 structure, malformed calls/content, and recursive forgery reject before
  traversal. `$search_space(values=)` accepts an ordinary or names/class-only S3
  named list without dispatch, enters this same admission, and replaces every
  live BASE candidate with a sealed single-use capability before closed
  conversion.
- Apart from the public-table and `set_values(.values=)` boundaries above,
  Domain/Condition/token/ParamSet and every other interpreted structural
  ALTREP/S4 shell is rejected. The outer `special_vals` list is ordinary
  non-ALTREP/non-S4 for every Domain kind. At typed Dbl/Int/Fct/Lgl
  construction ingress, a stable atomic, non-S4 ALTREP special leaf is
  materialized once by the canonical semantic-leaf owner and stored as an
  ordinary value. Operation-time masked admission rejects every typed ALTREP
  special found in a live Domain table without observing an element. Leaves
  retained by identity do not gain an ALTREP normalization path; an admitted
  typed S4 special, default, or init matches only by pointer identity. ParamUty
  leaves remain opaque, including S4, while Paradox-1 special membership alone
  uses base `identical()` without S3/S4 dispatch. Malformed exact-token/Domain
  structure is a hard boundary error, while ordinary value infeasibility
  remains a check diagnostic.
- ParamSet-family equality is a detached complete-state graph comparison and
  never walks private/inherited R6 active bindings. Canonical node references
  distinguish shared from duplicated topology without distinguishing
  independently built equivalent DAGs.
- Grid generation is one output-sensitive native graph operation. It shares
  dependency planning/comparison with Design masking, preserves exact ordinary
  first-nominal-occurrence order, and applies `upper_limit` to the final
  realized design rather than a nominal or intermediate product.
- All major compatibility breaks above ship now. They are not deferred to a
  later release.

Changing one of these requires an explicit contract/design/NEWS/test update,
not a local compatibility workaround.

## Implementation convergence and active-candidate acceptance

Checked entries below record implemented architectural components or
historical exact-payload conclusions. They do not by themselves make the
active candidate release-ready and do not transfer an earlier candidate's
green gates.

### Historical implementation and candidate milestones

- [x] converge the dormant-values/default-aware-activity implementation,
  focused contract tests, documentation, differential cases, and dependency-
  rich benchmark workloads;
- [x] replace nominal Cartesian grid materialization with the native
  output-sensitive fixed/dependency-aware engine, focused exact-order and graph
  tests, final-size ceiling, and collapse/pruning benchmark workloads;
- [x] normalize source references on package-interpreted callbacks and legacy
  migration while preserving opaque function-valued payloads and the
  admission-time debugging opt-out;
- [x] complete the pre-specified final performance batch with focused
  correctness, balanced A/B evidence, direct routine coverage, strict
  GCC/Clang builds, and the retained integrity-validation stop boundary;
- [x] complete R 3.6 compatibility and its header/runtime/portability harness;
- [x] repair the independently discovered active graph-frame GC lifetime defect
  with one managed carrier plus ordinary and compile-time-instrumented
  regressions;
- [x] freeze the now-historical replacement candidate at `dbbdcc1`;
- [x] run that candidate's eight-task `release-core` profile; all rows pass,
  while the native child source proof is retained only as informative execution
  evidence because its copied modes do not replay;
- [x] repoint the then-current exact `paradox2` compatibility axis at the
  frozen `dbbdcc1` ref/commit/tree and pass its structural profile fixtures;
- [x] create and independently validate a fresh replayable native source run
  with the repaired harness;
- [x] complete and audit exact-candidate rchk discovery, regenerate its
  source-bound policy, and add focused GCT coverage for the sole new
  branch-dependent protection-depth family;
- [x] use a fresh static/focused donor to complete that historical candidate's
  combined-memory gate;

### Current candidate acceptance

- [x] close the final independent focused adversarial review at the exact r9
  snapshot, including post-`Length` Condition admission, deterministic Domain
  special-name receipts, and the ordinary structural-name boundary;
- [x] freeze package-facing source at `de1752f`, including the C17 ceiling,
  dual-compiler C23 gate, seven supported-runtime stages, and focused admission
  corrections; its diagnostic release run passed every row except the common
  pre-test recursive-support staging failure;
- [x] commit the package-facing-source-identical recursive staging repair at
  `6f28dae` and exercise every supported-minor suite plus its support-tree
  receipts; that diagnostic run exposed the four bounded failure families and
  is rejected rather than promoted;
- [x] converge the focused encoding and fixture/policy repairs, freeze
  `fb2a37f`, and exercise every supported-minor suite with clean assertions;
  reject that diagnostic candidate because one list-ALTREP capability skip
  lacked its reviewed result-ledger row on the four pre-4.3 runtimes;
- [x] converge the reviewed leading-guard/four-row skip-policy repair and
  freeze diagnostic candidate `bf0b68f`; reject it when bounded rchk requires
  the source-level Domain-admission phase extraction;
- [x] converge the analyzer-only phase extraction without a production helper
  call or public-performance regression and freeze immutable candidate
  `4e549f3` at `refs/paradox-release/candidate-20260801T092108Z`;
- [x] repoint every then-active compatibility identity to exact `4e549f3`
  package identity before that candidate was superseded;
- [x] close the Domain receipt-compaction three-way timing obligation with the
  corrected eight-gate `a153fae` r5 evidence; its exact package fingerprints
  and semantic keys passed, and the bounded plan explicitly forbids rerunning
  this completed slice during final cleanup;
- [x] run the complete `release-core` profile for candidate `4e549f3`:
  current R 4.6.1
  full native/package acceptance, both explicit C23 compiler modes, all seven
  supported runtimes—exactly one complete stage for every minor line at R
  3.6.3, 4.0.5, 4.1.3, 4.2.3, 4.3.3, 4.4.3, and 4.5.2—all pinned header
  axes, and the differential gate. A complete supported-runtime stage means
  the full main source suite ran with `NOT_CRAN=true`, no
  `PARADOX_SKIP_CHARACTERIZATION_GCT` override, and no admitted `On CRAN`
  result; only exact source-derived runtime-capability skips remain;
- [x] use the authenticated bounded analyzer with the reviewed finite
  3,000,000-state main and allocator-discovery caps; obtain a complete
  active-source report, cover all 115 blocks through three disjoint independent
  source-review partitions, and commit its exact 396-UP/30-PB policy with no C
  defect found;
- [x] freeze the corrected Valgrind scope-receipt wording, create its fresh
  replayable donor, and complete combined GCT, Valgrind, bounded-rchk, and
  independent memory validation against `4e549f3`;
- [x] refresh the reviewed CRAN direct-consumer set to the exact 37-package
  `release-refresh-20260801-r2` proposal, authenticate every archive offline,
  and pin the exact Rush provider required by the current downstream heads;
- [x] install fresh candidate-owned
  `release-refresh-20260720` bridge overlays for both axes with the final
  ten-package profile, run all eight exact prepared-head source checks, and
  seal the recalibrated benchmark for the superseded `4e549f3` candidate;
- [x] close the hosted-Windows `R_HOME` reset, integer-ALTREP row-name,
  old-R fixture, and R-4.5 package-promise review findings; freeze replacement
  candidate `f27776e` at
  `refs/paradox-release/candidate-20260802T183338Z`;
- [x] run `f27776e` through all nine `release-core` rows, including current R,
  GCC/Clang C23, and one complete stage on every minor R series from 3.6.3
  through 4.5.2;
- [x] complete and source-review fresh bounded-rchk discovery for `f27776e`,
  regenerate its exact 116-block/397-UP/30-PB policy, and pass a fresh donor,
  combined GCT/Valgrind/rchk run, and independent memory replay;
- [x] fix the stale-candidate compatibility self-test before any consumer ran,
  freeze package-facing-source-identical tooling at `f711c67`, and construct
  fresh Paradox-1 and Paradox-2 profile overlays at the same exact dependency
  endpoint;
- [x] pass all eight exact prepared-head source checks on both axes, retain and
  review the complete 20-of-28 broad corpus and 10-of-22 reverse run, pass all
  mandatory documentation rows, and independently replay every evidence
  stage;
- [x] seal the final 82-row `f27776e` benchmark with 78 passes, four bounded
  marginals, and zero failures;
- [x] create and locally validate initial direct-child portability companion
  `198e838`; retain hosted run `30793059118` as failed-harness evidence after
  it exposed the R 3.6 top-level launcher boundary;
- [x] freeze the direct-x86-64-launcher correction and hardened regression
  tooling at `a2af703`, and locally validate replacement direct-child companion
  `ff3b510`; retain hosted run `30803703541` as failed-harness evidence after
  its command-discovery assertion stopped before R;
- [x] remove name discovery from the old-Windows execution boundary, freeze
  exact-path/helper-tree validation at `812e5ab`, and locally validate final
  direct-child companion `582eba8` plus its exact harness tag;
- [x] run and retain every applicable local gate against `f27776e`, including
  downstream, documentation, benchmark, runtime, and memory stages;
- [x] retain hosted run `30807910809` as deterministic old-Windows harness
  evidence after current Windows/macOS and the old-Windows package execution
  passed but the obsolete exact-one-NOTE classifier rejected the additional
  bounded-closure Rd-xref NOTE; do not retry its immutable companion;
- [x] converge the direct-value callback, recursive migration, retired-binding
  srcref, old-Windows Rd-xref, and old-GCC initializer fixes and freeze active
  candidate `a0a9ff3` at
  `refs/paradox-release/candidate-20260803T131049Z`;
- [x] pass all nine `release-core` tasks for `a0a9ff3`, including current R,
  both C23 compilers, and one complete suite on every R minor from 3.6 through
  4.5;
- [x] pass and independently validate fresh combined GCT, Valgrind, and
  bounded-rchk evidence for the exact `a0a9ff3` native donor;
- [x] freeze package-facing-source-identical `a0a9ff3` compatibility tooling
  at `f7b3eff`, construct both fresh axes, run focused Paradox-1 and complete
  Paradox-2 compatibility/documentation, retain the factual 20-of-28 corpus
  and 10-of-22 reverse conclusions, and independently seal benchmark r2 with
  79 passes, three bounded marginals, and zero failures;
- [x] create and locally validate initial exact one-path direct-child Windows
  x86-64/macOS ARM64 portability companion `da5a500`;
- [x] retain hosted run `30853585319` after all four jobs passed but offline
  evidence validation rejected the CRLF-converted old-Windows runtime lock;
  classify it as harness diagnostic evidence, not a package failure, and do
  not retry immutable companion `da5a500`;
- [x] freeze package-facing-source-identical portability repair tooling at
  `3f48d33` and locally validate replacement exact one-path direct child
  `00a24cb` plus tag `paradox-2.0.0-ci-a0a9ff3-harness-00a24cb`;
- [x] manually publish and execute companion `00a24cb`; hosted run
  `30941929181` and its retained offline evidence pass;
- [ ] complete the manual downstream/publication/release handoff.

### State and public model

- [x] v1 external-pointer capsule with ordinary protected truth and a
      session-local derived-state stamp in its address slot;
- [x] fixed eleven-field BASE/COLLECTION/SHADOW schema;
- [x] lazily refreshed derived schema: a collection flatten and a shadow
      projection follow the sets they are derived from;
- [x] one dangling-dependency resolution scope: `$deps`, checks, value reads,
      child constraints, designs, and samplers translate an unresolved parent
      outward through the same walk, and a shadow shows and enforces such a row
      exactly as its origin does while still refusing an edge across its
      visible/hidden boundary;
- [x] canonical plain internal table constructors/validators;
- [x] package-owned exported ParamSetShadow shell and initial bridge contract;
- [x] closed Domain and Condition public dispatch;
- [x] standalone Condition comparison and scalar/table constraint-only calls
  enter registered native operations with no S3 or R row-evaluation engine;
- [x] native collection-add and tag/dependency/callback mutation planners
  replace the remaining R/checkmate/data.table canonical mutation paths;
- [x] pure single-object legacy upgrader with CRAN-1.0.1 and `mbo_config`
  fixtures in the historical candidate;
- [x] recursive identity-preserving graph upgrader, versioned current targets,
  cold first-use gateways, and exact owner registry are complete and pass their
  focused source/fixture/adversarial checks on the now-frozen payload;
- [x] complete live Shadow synchronization, clone/serialization/DAG behavior,
  and all graph-reader coverage confirmed after converged install;
- [x] value, tag, dependency, callback, and collection-add mutators use
  validated capsule replacement/generation semantics; `assert_values` is the
  explicitly separate public shell policy;
- [x] no current object path reads legacy private tables as semantic authority.

### Single native engine

- [x] native Domain/ParamSet constructors replace former fast/slow constructor
  pairs;
- [x] constructor, ParamSet, and ObjectTuneToken Domain paths share the sole
  canonical built-in Domain-row semantic admission owner;
- [x] bounded value-producing Domain (excluding unbounded ParamUty and
  zero-level ParamFct) and exact BASE-only ObjectTuneToken
  admission, safe genuine-core aliasing, generation receipts/final commit scan,
  sealed search capabilities, ALTREP/S4 fail-closed structure, pointer-only
  typed-S4 special matching, and ParamUty base-`identical()` special membership
  are confirmed against the converged install;
- [x] unified BASE/COLLECTION/SHADOW `check` and `check_dt` implementation is
  integrated at source level;
- [x] standalone Domain and ParamSet scalar/table built-in admission use one C
  classifier and failure-only informative formatter, with no R/checkmate
  duplicate;
- [x] live and detached collection transformation/constraint factories use one
  registered native evaluator family, including subset, flatten, and
  Shadow-origin paths, with their final deterministic merge-order fix rechecked
  against the converged install;
- [x] native `check_dependencies()` and BASE-Shadow constraint-plan boundaries
  are integrated with focused graph, classed-input, callback-once, and
  malformed-state regressions; final combined-install evidence remains below;
- [x] values, domains, params, dependencies, transformations, subset/flatten,
  design, and sampler operations are capsule-authoritative and contain no
  semantic fallback or generated-R6 authentication; the documented cold
  internal-tuning and exact-TuneToken search-space families are the two R
  semantic-orchestration exceptions; cold clone and detached equality remain
  non-semantic shell/presentation glue;
- [x] all temporary former-auth aliases and obsolete translation units are
  deleted;
- [x] every registered routine has one fixed signature, direct probe, and
  synchronized coverage ledger.

### Tests and docs

- [x] contract-first design and compatibility documents replace conflicting
  old design guidance;
- [x] NEWS/DESCRIPTION/NAMESPACE begin the 2.0.0 contract reset;
- [x] all tests that assert superseded private/sentinel/S3 behavior are removed
  or rewritten, with preserved ordinary behavior still covered;
- [x] the exact focused ordinary-value matrix covers missingness, type/length,
  integerish, lower/upper bounds, factor membership/type mismatch, checked
  assignment, and Domain/ParamSet scalar/table message parity;
- [x] the historical candidate's complete capsule, graph, callback/reentry,
  structural-versus-semantic
  ALTREP/S4, direct-assignment versus `set_values(.values=)`, shared public-
  table classifier/row-name/cache/name-reentry, semantic-column, data.table
  facade, zero-column Design, corruption, serialization, exact-
  TuneToken/receipt/capability, and upgrade contract suite passes on the former
  frozen candidate payload, authenticated through the sealed `a4617ca` to
  `10c6a0e` package-payload equivalence proof and exact-candidate static/memory
  stages; this does not validate the active migration payload;
- [x] recursive migration tests cover identity, sharing/cycles, attributes/S4,
  environments/closures/bytecode, active-binding non-invocation, promise
  non-forcing, traversal boundaries, current-core payloads, full-preflight
  failure, monotonic retry, first-use modes, exact owner bridges/retired fields,
  authentic Paradox-1/downstream fixtures, and hostile malformed state;
- [x] package reference documentation, vignettes, migration guide, website,
  and downstream bridge docs describe the new migration behavior consistently;
  the active 17-workload documentation stage passes every mandatory row, while
  the unrelated `mlr3book` full-render and two legacy `mlr3gallery` dependency
  rows remain advisory exclusions;
- [x] historical routine/analyzer/runtime ledgers dynamically discovered their
  candidate files and contained no hard-coded test counts; the final old-R run
  staged the reviewed `mbo_config` Git-object bundle before worker admission and
  executed its upgrade test without an environment skip;
- [x] the registered-routine inventory and bounded-rchk policy are regenerated
  for the frozen graph routine; the exact discovery2 report is reviewed below;
- [x] R-API-exception, remaining symbol-audit, and runtime ledgers are verified
  for non-forcing promise inspection in the final exact-candidate gates.

### Downstream coordination

- [x] local bbotk bridge `4d49750` and miesmuschel bridge `d4c7f79` form the
  recorded pre-migration baseline;
- [x] bbotk adds the exact additive legacy-Codomain owner registration and
  miesmuschel adds the exact legacy-Shadow replacement registration, retired
  `params_unid`/`set_id` behavior, and any required owner-local cold gateways;
  both receive focused explicit/first-use migration tests on both Paradox axes;
- [x] the tested post-migration bridge trees are committed as bbotk `09dafa6`
  and miesmuschel `3c4bf94`; the former declares the exact rush development
  floor required by its direct `assert_profiles` call and the latter keeps its
  unavoidable dual-version
  namespace rebinding allowlisted and registers relocking before the first
  historical target is changed, and its dormant-value bridge regression passes
  both Paradox majors;
- [x] mlr3tuning `0ec4f40`, mlr3mbo `85dd8a5`, celecx `5a094a3`,
  mlr3pipelines `a795406`, mlr3fda `0df56f5`, and mlr3forecast `35e4bdc` are
  prepared on their recorded branches; bbotk, tuning, mbo, fda, and forecast
  are rebased on their current
  target heads, and celecx requires the exact bridge development floor before
  the intended mlr3mbo >= 1.2.2 release. Complete mlr3tuning suites and focused
  mlr3pipelines contracts and forecast snapshot files pass against both
  Paradox majors; the mlr3
  `35e30a9` and mlr3fselect `ae8e1d1` diagnostic-only branches are documented
  as obsolete and have no corresponding open PR;
- [x] the superseded pre-dormant profile heads were authenticated against their
  recorded candidate; the complete priority-zero/one Paradox-2 repository
  corpus and five-package source-check conclusions remain historical evidence
  only;
- [x] authenticate the refreshed snapshot/provenance manifests and exact
  `3c4bf94`/`a795406`/`35e4bdc` bridge heads in fresh candidate-owned
  ten-package overlays, then build and check all eight changed PR heads on
  both Paradox axes;
- [x] diagnostic-only downstream changes remain pruned and the current
  committed bbotk/miesmuschel owner bridges pass focused tests plus authentic
  default/opt-in migration fixtures against the then-reopened Paradox-2
  development payload;
- [x] the historical `8797f11` scoped consumer corpus and then-active
  documentation ran against their then-exact frozen payload and bridge heads:
  20 of 28 repositories were green, eight were reviewed
  non-Paradox/environmental exclusions, and all mandatory documentation rows
  passed; those counts are not active-candidate evidence;
- [x] user has manually pushed the eight retained exact branches and opened
  their draft PRs (agents have no remote-write authorization); marking ready,
  review, merge, and release remain manual.
- [ ] bbotk, mlr3tuning, miesmuschel, mlr3mbo, mlr3pipelines, mlr3fda, and
  mlr3forecast have released their dual-compatible revisions before Paradox 2;
  the GitHub-only celecx bridge follows mlr3mbo >= 1.2.2.

### Performance and correctness

The exact frozen candidate's source-bound bounded-rchk discovery is under
`.local/checks/serialized-migration-release-8797f11-memory-20260724/modes/rchk`.
It analyzed 870 functions and 30,245 states, with 80 reviewed Function blocks,
238 UP diagnostics, and 13 PB diagnostics. Its original raw bcheck report
SHA-256 is
`02b08085ea0fadc906fb8e8fdd3f5211a6eb2a7eb08e922205e69f4d25361072`.
The final combined memory run under validation tooling
`a05cd51a5570c4a674b6c80d6cd38c7898223635`, tree
`c58b6bea97d66e23b542a34864479e28c5e5e02f`, produced raw bcheck report
SHA-256
`0226275247eb16736ab317dfb3e1c7f836ee006fb59683276998632aa120cd9d`
and the same reviewed inventory with ordering-insensitive semantic SHA-256
`f3dc5caccc4508f9f9263d8d912455820dfba428cb00f7d0454e1734adf6da18`.
The original and final raw reports are not byte-identical; their sealed
semantic comparison, not a raw-byte claim, supports reuse of the review.
Maacheck is byte-empty
(`e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`).
Fficheck reports 79 registered functions and one checked registration call
(`565392164712e15df4bbdd0b34fb852b35eeab380f612c0983f2ccab69c31370`).
The final policy, block table, and unchanged rationale table SHA-256 values are
`e6010f58c58dfb8e952ee0e515a1a2352decd143e01bda50af7c800b4aa0470d`,
`50445fd2be3da7cbeb05f689377802deac7597ee2eeed8acc34f683989e3a71d`,
and `c9e94a9f49b5570838df94fba7f45ef870999b78d03b7b4824412dc34645d8a4`.
The final combined GCT/Valgrind/rchk completion SHA-256 is
`a07df9509c78976d6d8a31a9b4f3415787f512e73bf85a2311d6ced672e9b226`.
The `a05cd51` tooling diff is package-facing-source identical to the candidate;
it is not described as package-identical.

The preceding discovery exposed a real protection imbalance in
`schedule_vector()`. R explicitly permits an ALTREP `Duplicate` method to
return its input, but the former implementation protected both the input and
duplicate result and then used pointer inequality to decide whether to release
the second protection. The replacement keeps one indexed root and uses
`REPROTECT`; the candidate also contains a test-only ALTREP list whose
`Duplicate` method returns itself and a graph-discovery regression that captures
R's direct stack-imbalance diagnostic. `schedule_vector`'s former one-UP/two-PB
block is absent. The 80 current blocks and rationale assignments are the exact
reviewed inventory. The retained `a05cd51` combined memory completion, not
discovery alone, closes the release gate.

The following historical results and hashes bind the superseded pre-migration
payload. They remain useful engineering evidence but do not close a release
gate for either the last sealed candidate or the reopened cleanup source.

The historical candidate's rchk policy bound the refreshed reviewed
public-table source and its bounded-analyzer reports. Bcheck
analyzed 782 functions and 28,140 states, with 77 exact Function blocks, 196 UP
diagnostics, and 13 PB diagnostics; its report SHA-256 is
`4a405e12807da7ee5347a6ad610530fc41b0f5331bd65221285399fcc2e12655`.
Maacheck is byte-empty (`e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`),
and that source's fficheck reports 72 registered routines and one checked
registration call
(`92364873a511f8bd20e0f64854db8ed749d20b9e5d5d6acb730dcb3a045da39f`).
The generated policy, block table, and rationale table SHA-256 values are
`d4b4c38a683b4e6f5110bf83d41eb4a0909a2d42fbea1b25723ed0d4265a46c1`,
`a9b33fb5d53180fc1e1d688cc0b7c5a549d03d901ba199d78ae6a35927132d4b`,
and `c9e94a9f49b5570838df94fba7f45ef870999b78d03b7b4824412dc34645d8a4`.
The first pre-freeze report exposed a real `snapshot_dependencies()` root-
lifetime defect across callback-capable feasibility validation. The result and
its columns now remain protected through that validation, a successful
allocating-callback regression covers the commit path, and the superseded raw
run was discarded before generating its policy. The refreshed policy includes
the registered `$has_deps` reader; its one address-taken graph-root diagnostic
is reviewed as `ADDRESS_TAKEN_MODEL`. The public-table classifier added three
registered diagnostic routines and shifted only analyzer-generated helper
suffixes and line locations; the reviewed UP/PB block inventory and rationale
assignments are unchanged. The exact frozen-candidate memory gate retained the
same authenticated report and passed Gctorture, Valgrind, and bounded rchk.

The checklist below records historical R-3.6 candidate progress across
superseded payloads. It is retained to explain old evidence and is not the
acceptance state of reopened package-facing source:

- [x] strict C99 compilation against R 3.6.0 and every later pinned header/API
  branch;
- [x] an authenticated real R 3.6.3 source-library build/install/test stage,
  including its complete-test closure, the precise active-binding and
  list-ALTREP capability results, and the exact bounded
  four-missing-Suggests package-check NOTE;
- [x] the separate exact R 3.6 declared-floor install/smoke authenticates every
  package identity and dependency namespace origin plus the candidate Paradox
  DLL, with ambient `R_DEFAULT_PACKAGES` isolated;
- [ ] a fresh complete seven-runtime selection (R 3.6.3 plus every R 4 minor
  through R 4.5.2) seals the exact
  R 4.0.5-to-R 3.6.3 current-v2 serialization handoff; a partial selection
  makes no cross-runtime claim;
- [x] the source-derived bounded `NOT_CRAN=true` GC/reentry slice passes all
  exact selected targets under both R 3.6.3 and R 4.0.5, including the
  selector isolation self-test and independently regenerated title filter;
- [ ] an authenticated hosted Windows x86-64 R 3.6.3/Rtools35 source-build,
  PE-DLL load/registration, and focused smoke/check artifact;
- [x] the current-R full native lane verifies both sealed ConfigSpace
  environments and all locked local CRAN/BioC repository indexes before and
  after its networkless, read-only test and package-check execution; its
  functional result is green, but a fresh replayable source proof is still
  required for memory;
- [ ] the complete applicable current-R, portability, memory, compatibility,
  documentation, and benchmark gates after source freeze.

- [x] directly affected development tests pass from stable cached
  installations;
- [x] the complete unit suite, CRAN-style package check, and depends-only check
  pass for the historical frozen migration payload under
  `.local/checks/serialized-migration-release-8797f11-20260724`;
- [x] for the historical sealed migration payload, profiling was closed:
  sparse search-target projection and a bulk-dependency constructor
  transaction were measured no-gos for 2.0.0;
- [x] measured hot-path changes remain intact under the bounded refreshed
  comparison recorded below;
- [x] the historical sealed-candidate benchmark has 77 policy/decision rows:
  73 pass, four bounded marginal reviews, and zero failures;
- [x] strict GCC/Clang, both analyzers, cppcheck, symbol/registration audit,
  ASan, UBSan, and R-API/exception-ledger checks are sealed for the historical
  payload;
- [x] the combined GCT, Valgrind, and bounded-rchk memory completion is sealed
  for the historical payload;
- [x] real R 4.3.3 and 4.5.2 runtime execution plus the current local R 4.6.1
  native execution are sealed for the historical payload;
- [ ] Windows x86-64 and real macOS ARM64 are clean for the exact frozen
  candidate and independently retained;
- [x] priority consumer, documentation, differential, and benchmark gates are
  rerun and accepted for the historical payload.

Current profiling diagnostics are implementation guidance, not release
benchmark evidence:

- the bounded migration-payload comparison in
  `.local/benchmarks/migration-hotpaths-20260724/` ran 200 samples after 10
  warmups for eight representative paths. Seven improved by 4--24% and all
  eight retained identical allocation counts. The sole `ids()` shift was under
  3 microseconds, its R and C implementations are byte-identical to the
  superseded candidate, and a current/current process comparison showed 7.2%
  variance, so no destabilizing source change was made. Authentic legacy graph
  scaling under `.local/benchmarks/migration-scaling-20260724/` is flat through
  128 aliases and linear at roughly 18--20 ms per distinct shell;
- the operation-local SHADOW ID index is retained in
  `.local/benchmarks/dev-shadow-pointer-index-ab-20260718`; its A/B medians
  improved construction by 1.39x, live values by 7.72x (293 to 38 microseconds),
  live domains by 2.17x, and assignment by 1.70x while keeping complete
  corrupt-state validation;
- the direct base checks in the `to_tune(ParamSet)` callback wrapper measured
  4.47 microseconds versus 24.31 microseconds for the former
  checkmate/mlr3misc layers in the focused probe. The final paired gate confirms
  the representative end-to-end workloads on the frozen candidate payload;
- an earlier isolated `check_dependencies()` and BASE-Shadow constraint-plan
  stage compiled with the complete strict C17 warning set under GCC 14 and
  Clang 22 without a diagnostic. Its immutable GCC installation (DSO SHA-256
  `ff1755578568856af74b11f868a7da981b13395c974329f2096ea7e31f76b456`)
  first passed 35 focused test blocks with 241 expectations. After adding two
  test-only first-diagnostic assertions, the unchanged installation reran the
  affected dependency file (14 blocks and 111 expectations), leaving 243
  focused expectations for that source. Its 55 registered calls, 55 direct
  probes, and four hazard probes passed. Evidence is retained in
  `.local/tmp/native-semantic-leftovers-20260718/`;
- the subsequent isolated standalone-Condition and constraint-only stage (DSO
  SHA-256
  `b8de34b72ee5fac27ade0777058f7a7a70ac1d4e7c5d33c0cff025a44f313c4e`)
  passed five affected files, 37 test blocks, and 243 expectations, plus 58
  registrations, 58 direct probes, and four hazards. GCC 14 and Clang 22 were
  warning-clean. Its 10,000-row constraint batch measured 0.0148 seconds per
  call versus 0.511 seconds for the old R row engine, a 34.5x improvement.
  Evidence is retained in
  `.local/tmp/native-constraint-stage2-20260718/`. A later measured
  `CondEqual` pointer fast path and the merged native mutation/add operations
  changed the source again. The vector fast path is now guarded by the
  permanent `condition_equal_vector` workload and regression-policy row;
- a final collection-reader profile attributed about 86% of the representative
  rich-read instruction count to complete graph admission. Within that required
  validation, encoding translation and affixed-ID comparison were measured hot
  spots. Two conservative byte fast paths were retained: equal UTF-8/Latin-1
  encodings and native ASCII compare without translation, while mixed encodings
  and non-ASCII native strings keep the UTF-8 path. With 5,000 samples pinned to
  one CPU and both execution orders, the shared string change improved rich
  reads by 1.127--1.131x and nested reads by 1.253--1.294x; the affixed-ID change
  then improved plain reads by 1.105--1.121x, rich reads by 1.083--1.104x, and
  nested reads by 1.106--1.115x. Allocations were unchanged. The tested stage
  DSO was
  `40784de682305cd1ca7322b375e5504aa3b483428944b149e9d6958e47503ea7`;
  retained A/B evidence is under
  `.local/benchmarks/collection-values-ab-{string,affix}-long-20260719` and the
  corresponding `-reverse-` runs. Translation caches, alternate validation
  modes, and skipped corruption checks were rejected;
- a fused Shadow-values reader experiment was also rejected and fully reverted:
  100-sample exact A/B medians were 60.345 versus 60.205 microseconds (1.002x)
  with identical 2,200-byte allocation. It added a routine and duplicated
  reader surface for no material gain. Evidence remains under
  `.local/benchmarks/fused-shadow-values-ab-20260719`;
- the final low-hanging pass retained four compact changes: skip an empty value
  transaction when a ParamSet has no initial values, reuse the already resolved
  BASE row while translating admitted collection values, and let the inherited
  native Shadow dependency reader own refresh. It also replaces the
  `$has_deps` dependency-table/data.table projection with the registered scalar
  reader described above. Forward/reverse paired evidence for the first three
  is retained in `.local/benchmarks/final-hotpath-ab-20260719` and
  `.local/benchmarks/final-hotpath-ab-reverse-20260719`. Small construction was
  4.3--6.5% faster with 880 fewer allocated bytes; 64-parameter bulk
  construction was 8.0--12.3% faster with 1,744 fewer bytes; rich collection
  reads were 4.6--5.8% faster and nested reads 18.1--18.8% faster. Plain reads
  remained within 1% timing noise. Collection reads used 192 additional
  operation-local bytes. The production delta was 21 source lines and 320 DSO
  bytes, with no persistent cache or weaker validation mode. The separate
  `$has_deps` A/B evidence is retained under
  `.local/benchmarks/has-deps-scalar-ab-final-20260719`: 50 evaluations after
  three warmups moved the median from 498.575 to 156.065 microseconds (3.195x),
  with the same 12,688 bytes in 14 `Rprofmem` records on each side;
- the paired release policy now records the unavoidable major-version integrity
  cost rather than treating it as an ordinary hot-path regression. Exactly
  seven rows use integrity tiers. `shadow_values_live` receives the finite
  `integrity-shadow-read` median/q75 ceilings 5.50/6.00. The former
  3.25/3.50 limit was calibrated before final exact same-pointer Shadow
  signature authentication and typed public-value detachment; the replacement
  limits retain a marginal decision for the measured final contract while
  still rejecting the retained early 16.95/17.80 stage. The three synthetic
  `collection_values_{plain,rich,nested}` rows and the three real consumer
  `$values` rows for `mies_mutator_maybe`, `mies_optimizer`, and
  `mlr3pipelines_graph` receive `integrity-collection-read` ceilings 2.75/3.00.
  Post-index Shadow profiling measured 2.623/2.605; the final-focus rich
  collection diagnostic measured 1.546 at the median, while the last common
  pre-final-fast-path nested diagnostic measured 2.361/2.349. The collection
  ceiling still rejects the retained pre-optimization 3.365/3.591 stage.
  Consumer `$params`, `get_values_unchecked`, filtered getters, domains,
  dependencies, mutation, and all other real consumer operations keep their
  strict ordinary tiers; the integrity rows also retain the `hot` allocation
  budget.
  The sealed release benchmark subsequently recorded 73 pass, four bounded
  marginal reviews, and zero failures. The four marginals are
  `shadow_values_live` timing plus allocation for
  `collection_values_{plain,rich,nested}`; the three real consumer `$values`
  integrity rows pass;
- a sparse-target `$search_space()` facade experiment was rejected. The
  conversion is cold and the representative maintained end-to-end workload
  moved only about 2%, which did not justify an additional projection path and
  validation surface;
- a native bulk-dependency constructor transaction was also rejected for this
  release after measurement. The isolated 64-parameter/27-requirement estimate
  moved from 6.57 ms to 4.11 ms, with requirement-heavy estimates spanning
  roughly 1.4--2.5x, but representative xgboost learner construction improved
  only about 6--7%. Implementing it requires a moderate-risk new native batch
  transaction, and the maintained release workload currently lacks dependency-
  rich constructor coverage. Under the release-steering policy this is not
  low-hanging enough to reopen the implementation. The existing low-risk wins
  remain; this internal optimization can be reconsidered later without another
  compatibility/API break and is not included in any claimed speedup above;
- none of the staged DSOs above is current combined evidence. The candidate
  table and retained-evidence record below name the immutable source and the
  applicable release gates; those final rows, not the development diagnostics,
  support the local release conclusion.

### Historical informative-diagnostic focused evidence

The informative-diagnostic source before the object-graph migration had bounded
evidence appropriate to that change. It is historical for the current payload
and is not a replacement for the complete release matrix:

- the ordinary GCC 14 development install has DSO SHA-256
  `2d1636ab5e0c10e23336f7bf33389e5963facdfaa08f7e23e2dd84372ecc4b49`;
- 68 ordinary Domain, ParamSet, constructor, and checked-assignment diagnostics
  are byte-identical to Paradox 1.0.1.9000, including cross-storage scalar
  missing values. Both retained TSVs have SHA-256
  `1d466842f042b60769a65d49aea54e95f4ff17f8cc8b592e1a7ded42343e3307`
  under
  `.local/checks/informative-diagnostics-final-differential-20260723/`;
- the affected package tests, exact constructor/assignment assertions, and a
  bounded `gctorture2(10, 1, 0)` formatter/admission loop pass. Strict GCC 14
  and Clang 22 C17 warning-as-error builds, the bounded analyzer corpus, all
  registered direct probes, and all four callback/allocation hazards pass under
  `.local/checks/informative-diagnostics-final-native-r2-20260723/`;
- a CPU-pinned randomized comparison against frozen `10c6a0e` found no material
  accepted-path regression. Raw current/baseline ratios were 0.988 for mixed
  `$check()`, 1.008/1.044/0.962/0.998 for double/integer/factor/logical
  `domain_check()`, and 1.056/1.040 for `p_dbl()`/`p_int()` construction, while
  the process-local control itself was 1.073x slower;
- the same focused six-consumer matrix passes 2,022/2,022 expectations with
  zero failures, errors, warnings, or skips on each of Paradox 1.0.1.9000 and
  the final Paradox 2 development DSO. Evidence is retained under
  `.local/checks/informative-diagnostics-downstream-paradox1-focused-20260723T131726Z/`
  and `.local/checks/informative-diagnostics-downstream-final-20260723T130129Z/`.

## Current downstream handoff branches

These are the exact final published handoff heads after the object-graph
migration, owner-registry bridges, informative native diagnostics,
dormant-value adaptations, and pruning of redundant downstream changes. Their
intended dual-version development tests and exact final source-package checks on both
Paradox axes are complete. All eight retained PRs are open, mergeable drafts at
the exact heads below; the user must mark them ready, review, merge, and
coordinate their releases. The obsolete mlr3 and mlr3fselect branches are
evidence only, have no open PR, and must not be published as replacements.

| Package | Worktree | Branch | Commits | Intent |
|---|---|---|---|---|
| miesmuschel | `.local/compat/github-release-refresh-20260720/miesmuschel` | `codex/paradox-paramsetshadow-bridge` | head `3c4bf94788b9259878b1fa067d216823d0771681` | Official ParamSetShadow/public-state bridge, Paradox-1 construction, cache-independent comparisons, dual-major Rd links and diagnostics, exact replacement registration, retired `params_unid`/`set_id` contract, bounded owner-local cold gateways, and a dual-version dormant raw/filter/reactivation regression. |
| bbotk | `.local/compat/github-release-refresh-20260720/bbotk` | `codex/public-paramsetcollection-sets` | head `09dafa6c3048f9be5b6961739787d201f6600a6f` (base `74515a792243a0f62a95f8ba3452be6290278c8c`) | Public `.sets` migration, detached-snapshot rooting, exact additive legacy-Codomain inspector/rebuilder registration without private state access, and an explicit `rush >= 1.2.1.9000` floor for the directly used compute-profile API. |
| mlr3tuning | `.local/compat/github-release-refresh-20260720/mlr3tuning` | `codex/paradox2-dormant-values-current` | head `0ec4f40033a393d41c7842541c2d5f8173dfb6bd` (base `5ac566dc53480e2fd3fa0497f70f2cb038412863`) | Test-only adaptation preserving strict NoDefault point checks and version-gating the changed TuneToken-child dormant contract. |
| mlr3mbo | `.local/compat/github-release-refresh-20260720/mlr3mbo` | `codex/paradox2-transformless-subset` | head `85dd8a5ada86aacafe93637711e3b1f2e91ba219` (base `4471f6fc4a8aa217fffb6ce5a45d3e525e96dc44`, runtime change `185b2298216eef47b0f976667c4e8949c069dff4`) | Use public `subset(..., keep_trafo = FALSE)` on Paradox 2 while retaining Paradox-1 paths, removing the obsolete set_id-era branch, and documenting the current release. |
| celecx | `.local/compat/github-release-refresh-20260720/celecx` | `codex/paradox2-diagnostics` | head `5a094a391ae11a8ae23ce4abf98eaf63e36bb3f1` | Retain only the independent cycle/dependency adaptation, require the exact compatible mlr3mbo development bridge (`>= 1.2.1.9000`) before its intended >= 1.2.2 release, and remove obsolete NULL writes to retired active bindings. |
| mlr3 | `.local/compat/github-release-refresh-20260720/mlr3` | `codex/paradox2-diagnostics` | obsolete head `35e30a9` | Do not publish; removing its numeric-diagnostic gates leaves an empty effective diff and no corresponding PR is open. |
| mlr3fselect | `.local/compat/github-release-refresh-20260720/mlr3fselect` | `codex/paradox2-diagnostics` | obsolete head `ae8e1d1` | Do not publish; removing its feature-fraction diagnostic gate leaves an empty effective diff and no corresponding PR is open. |
| mlr3pipelines | `.local/compat/github-release-refresh-20260720/mlr3pipelines` | `codex/paradox-diagnostic-compat-current` | head `a7954067061f20a45dd9e6c03129dca0ba0f1753` (base `bef040ae5c886bf5b09863b956d341eb3cbd772c`) | Retain only the GraphLearner deep-clone ownership fix, mutation-isolation regression, dual-version dormant spline contract, and symmetric inflation of the dictionary comparison shell. |
| mlr3fda | `.local/compat/github-release-refresh-20260720/mlr3fda` | `paradox2-snapshots` | head `0df56f51b5d7fd751e16575fbd897b1c7f449c5e` (upstream base `8960c9292221e7065e5175762e12354c6eb08607`; previous bridge head `c1cdad5a78913c9a47fec1003de8d4309275c80c`) | Preserve byte-identical Paradox-1 messages; the four Paradox-2 headers name `.__paradox2_ParamSet__values()` while their diagnostic bodies remain unchanged. |
| mlr3forecast | `.local/compat/github-release-refresh-20260720/mlr3forecast` | `paradox2-snapshots-20260801` | head `35e4bdc914a913508450866e629309f8364077ec` (base `8e352550f2334e42c8c81ee80d3e237b807abebe`) | Preserve byte-identical Paradox-1 messages; the three Paradox-2 Lags/Rolling headers name `.__paradox2_ParamSet__values()` while their diagnostic bodies remain unchanged. |

A final read-only live-head audit does not add another mlr3fselect probe. Its
current `29fa095` delta changes the sole Paradox-specific branch by deleting
the obsolete `set_id` test and calling the public `p_dbl()` sugar directly;
the pinned head already executes that same path on Paradox 2, and public
`p_dbl()` is covered on both axes. The remaining live-head changes are not
Paradox-facing and its new test corpus is coupled to mlr3's unrelated
`pima`-to-`diabetes` task replacement. Updating only mlr3fselect would therefore
create an invalid mixed snapshot, while updating both repositories would add a
broad unrelated corpus refresh rather than a cheap compatibility probe.
The paired mlr3 audit compares pinned `f70c001` with current `7cb6a08`: its two
commits and 67 changed files are confined to a `TaskClassif` print cache and
that data-task replacement across code, documentation, snapshots, and tests.
No Paradox import, ParamSet/value path, or Paradox-facing test changes, so the
smaller paired support snapshot remains the intentional release subject.

The last pre-dormant Paradox-2 source-package stage built and checked bbotk,
miesmuschel, mlr3mbo, celecx, and mlr3fda from its then-exact Git archives;
all five rows and retained statuses were green. It does not authenticate the
current `3c4bf94` miesmuschel head. Its Paradox-1 source-package conclusion was
an explicit composition. The r2 stage passed the exact then-final bbotk,
mlr3mbo, and celecx heads plus mlr3fda base `8f5a3df`; final mlr3fda `c1cdad5`
changes only four call headers in
`tests/testthat/_snaps/paradox-2/PipeOpFDAWavelets.md`, so its Paradox-1
selected tests and runtime source are unchanged. The final miesmuschel row
for that historical profile passed in r3. The r2 stage's obsolete miesmuschel
row failed, so neither that whole stage nor the final mlr3fda head is described
as an exact five-head Paradox-1 check. A fresh stage must bind the current
manifest before release.

The handoff in `compat/downstream-pr-handoff.md` records the final retained
heads, manual push/PR text, and the two redundant PRs to close. For historical
context only, the same focused 2,022 expectations passed with zero failures,
errors, warnings, or skips on both superseded Paradox axes under
`.local/checks/informative-diagnostics-downstream-paradox1-focused-20260723T131726Z/`
and `.local/checks/informative-diagnostics-downstream-final-20260723T130129Z/`.
Those runs and the earlier `bf64490` to `9e87556` test-only composition belong
to the superseded payload and do not authorize the active result. The active
`release-refresh-20260720` profile has separate candidate, tooling, overlay,
axis, and source-check receipts. A check is green only when its retained final
status has no ERROR or WARNING, independently of process exit status.

## Development evidence policy

While source is changing, record only focused diagnostic results. A source
parse, strict translation-unit compile, or focused test is useful development
evidence but not a release gate. Broad checks are deliberately delayed until
the architecture converges so that package/dependency binaries are not rebuilt
and entire suites are not rerun for each isolated failure.

Use one stable copied source tree and one disposable installation for a
coherent batch. Run independent test files/consumer rows in memory-aware outer
parallel waves, with nested compilation/test/BLAS/OpenMP at one. Mine every
failed wave for its full failure set, fix the shared cause, rerun affected rows,
then perform one final broad confirmation.

Authenticated toolchain, package-download, dependency-library, consumer-install,
reference-source, header, analyzer-runtime, and container caches remain valid
when their byte-affecting inputs match. A candidate DSO, package installation,
memory report, differential, consumer result, documentation result, or
benchmark is not transferable across a changed distributable payload.

The 2026-07-25 unknown-parameter-suggestion cleanup has development evidence
only: an incremental package installation; warning-as-error GCC 14 and Clang 22
C17 compiles of the changed translation units (also independently reviewed
against R 4.3 headers); focused `NOT_CRAN=true` runs of the new suggestion,
native ParamSet check, native value-mutation, and characterization
value-mutation files; and 500 randomized comparisons with the intended
Paradox-1 `mlr3misc::did_you_mean()` policy. All passed. No broad, downstream,
memory, or release benchmark gate was rerun for this isolated item.

The dormant-values/default-aware-activity batch follows the same development
boundary. During implementation, strict changed-unit compilation and focused
dependency/value/check/constraint tests are useful diagnostics only. The full
unit, compatibility, runtime, memory, documentation, benchmark, and hosted
portability gates are intentionally deferred because additional pre-release
todos remain. Any focused result must retain its actual source identity and
must not be entered in the candidate evidence tables below.

A completed package-facing row may be reused across refs only through a sealed,
independently replayed proof that every Git change is excluded by the exact
`.Rbuildignore` and that clean builds have the same complete payload inventory
and bytes after removing only R's generated `Packaged:` record. The donor run
keeps its original identity; the target ledger names the proof and transfer
scope. Tooling/policy/docs/profile/benchmark/portability inputs are not covered
by package-payload identity. A downstream test-only change reopens that
package's affected rows, not consumers whose head and production source remain
unchanged.

For an R/docs-only inner-loop change, a development DSO may be reused only
after recording byte identity of every native build input plus compiler/profile,
`NAMESPACE`, and `DESCRIPTION`, reinstalling the R/help databases, and verifying
the loaded DSO hash. This exception is diagnostic-only. The final immutable
distributable payload receives one clean full source build per executed
R/compiler/instrumentation profile; compatible evidence families may share
that exact authenticated installation, and a sealed identical-payload ref may
inherit the donor conclusion, never development component objects.

## Historical `dbbdcc1` sealed candidate freeze record

The previous immutable package-facing candidate was:

| Field | Value |
|---|---|
| Full candidate ref | `refs/paradox-release/candidate-20260727T152133Z` |
| Commit | `dbbdcc156cb52793e84e8767f0ce84b6ecbb85ea` |
| Tree | `b60b75e3923cdcb49f1ef2fb0f9b386d5cac291d` |
| Detached source | `.local/compat/candidate-snapshots/dbbdcc156cb52793e84e8767f0ce84b6ecbb85ea` |
| Candidate content SHA-256 | `08d646219c044a70225ca35bffacf7bb6213853e0d10536c0e45e65bf4219979` |
| Version | 2.0.0 |
| Release-core coordinator | `release-candidate-dbbdcc1`; all eight rows passed |
| Replayable native source donor | `release-candidate-dbbdcc1-native-replay-r2-native-release-a001` |
| Combined memory gate | `release-candidate-dbbdcc1-memory-r3`; GCT, Valgrind, and reviewed rchk policy passed |
| Portability companion | pending fresh direct child; candidate tag `paradox-2.0.0-ci-dbbdcc1` |

The historical transfers recorded for this candidate were limited to reviewed
commits whose exact diffs changed only validation tooling, evidence ledgers, or
downstream manifests under package-excluded roots. Later package-facing changes
reopened source and invalidate those transfers for the next candidate. Do not
call generated archives byte-identical without a separate sealed payload proof.

## Historical pre-cleanup candidate freeze record (superseded payload)

The sealed immutable package candidate below became historical when the
2026-07-25 cleanup reopened package-facing source:

| Field | Value |
|---|---|
| Full candidate ref | `refs/paradox-release/candidate-20260724T105215Z` |
| Commit | `8797f1163fe612cb01d1facf517834d3f516a697` |
| Tree | `81e6f901266754b97a0906f88a472bf04795f13c` |
| Candidate content SHA-256 | `ced2390bc756b01805e7bdcf32fdb4a6ff2c1bd010dd3d576194d939e491f644` |
| Version | 2.0.0 |
| Exact bounded-rchk discovery | `.local/checks/serialized-migration-release-8797f11-memory-20260724/modes/rchk` |
| Final memory tooling | commit `a05cd51a5570c4a674b6c80d6cd38c7898223635`, tree `c58b6bea97d66e23b542a34864479e28c5e5e02f` |
| Final documentation/consumer/benchmark tooling | commit `fc92edd7f1ab612468066fe06bd3d9fc7afea41c`, tree `05cc4e5213c5ee73d0bc764c3d102c15e4c57141` |
| Portability companion | `refs/paradox-release/portability-harness-5305ead`, commit `5305eaddbc9c2159fe194e6be10388c17b4c506a`, tree `e1fe00ddad08af89566e3df12460bc47d3e98292` |

This ref froze package source, package tests, help, and package-facing
documentation at that point. The exact candidate-to-`a05cd51` and
candidate-to-`fc92edd`
diffs contain no package-facing path; these relationships are
package-facing-source identity, not claims of complete package-payload byte
identity. The later cleanup source is not identical to this payload. Its native,
R API, runtime, combined-memory, differential, downstream, documentation,
benchmark, and hosted portability conclusions must be established after a
replacement candidate is frozen.

## Historical candidate freeze record (superseded payload)

The table below records the superseded `10c6a0e` payload only. It does not
describe the last sealed candidate above and authorizes no current release
conclusion.

An immutable package candidate is committed after package implementation,
tests, help, and package-facing documentation converge and the primary checkout
is clean. A commit cannot contain its own commit, tree, or archive identity
without a circular mutation. Therefore the candidate's own copy of this table
and the axis registry is necessarily pending. This post-freeze ledger records
the resolved identity below. The package/release tag continues to point to the
candidate, never to validation infrastructure, the final evidence ledger, or a
portability harness.

Post-freeze validation infrastructure may populate exact candidate/profile rows
and repair validation-only drivers in `AGENTS.md`, `benchmarks/`, `compat/`,
`design/`, `environment/`, and `scripts/`. Each such commit must remain clean
and prove that its diff from the candidate changes no package source, package
tests, help, or package-facing documentation. Source-bound results still name
the managed detached candidate, while each validation result separately records
the tooling commit/tree/status that produced it. Ordinarily freeze one final
validation-tooling commit and reuse its named overlay read-only. The recorded
release composition is narrower and explicit: documentation and benchmark used
`bf64490`; `9e87556` changes only the final miesmuschel test head/profile ledgers,
so new final overlays and the affected miesmuschel rows were built on both axes
while unaffected conclusions retain their original identities. This is the
non-circular model for the `release-refresh-20260720` profile, not authority to
mutate candidate bytes, replay arbitrary older tooling evidence, or relabel
execution.

After the remote gate and publication handoff complete, the final evidence-
ledger commit changes only this file and changes the decision from pending to
accepted. The portability companion changes only
`.github/workflows/r-cmd-check.yml` and the package-excluded old-Windows
installer helper. Creating either reopens only its own structural and ledger
checks. Any package-facing post-freeze change requires a new candidate and new
source-bound evidence.

| Field | Value |
|---|---|
| Full candidate ref | `refs/paradox-release/candidate-20260720T053518Z` |
| Commit | `10c6a0e65910206c8face91dac6c3dd1115e0bed` |
| Tree | `a205205194f0bc62114106504853721f678fa340` |
| Detached source | `.local/compat/candidate-snapshots/10c6a0e65910206c8face91dac6c3dd1115e0bed` |
| Clean `R CMD build --no-manual` archive SHA-256 | `917ea2a497f9e80ce4cf1d10c081cea8914d36eb7deb647c036dca49994ad558` |
| Deterministic Git archive SHA-256 | `0a712fa5c5f572ca2f2968fda211973fe0bede45b80a7fcee0848af863b1d7ca` |
| Normalized 217-file package-payload manifest | `e6e767b8fa3cd1a9273d62039c208d7c3ae1aea0b12d7446560297d4750dcc3e` |
| Version | 2.0.0 |
| Tracked source files | 486 |
| Routine inventory | 72 rows; `environment/native-routine-coverage.tsv` SHA-256 `bc7a9b382e62372954b9e197906d30733e37147b5f09c82ebf371b63cf30fa4c` |
| Test inventory | 84 `tests/testthat/test*.R` files; exact `mode/content-SHA-256/path` slice SHA-256 `8c7118167fe1e82e6c756661bf5a3a13d3795093e4c11a9982f3da9392551ebf` |
| Final downstream-profile tooling | commit `9e875567ef0462e659906dd6aa0acfdc8fba3044`, tree `107ce3e060932000701d872024494971878675f8` |
| Downstream bridge manifest | `compat/github-bridge-provenance.release-refresh-20260720.tsv`, SHA-256 `79596dc32d9030e4b86bfc13310bad277fe9070577a2ff703b295262378e9e5f` |
| Portability companion | `paradox-2.0.0-ci-10c6a0e-harness-cc06c18`, commit `cc06c182949af09ce80e335ddbfc63a8078692e6` (remote evidence pending) |

The eight exact bridge heads are bbotk `6cae9559cfa2133b02b19e9762211aa49ec4c1c7`,
mlr3 `35e30a91e305936e57328b65e15b60f3ab00eef3`, miesmuschel
`d4c7f79750cd15c8174415fb0ba059c597f4f055`, mlr3pipelines
`1c4bc6e52005d40d61fdba27b047f09fd6a6d29a`, mlr3fselect
`ae8e1d163bc7d8a2dd9f12e61d704e5b0d8430d7`, mlr3mbo
`1a1c0abe95f59cd314f1fbc19c596cb6ac15f067`, celecx
`a2975550c14f824c6abc86db9db32e982908c3ea`, and mlr3fda
`035da5bb8d1c2ae22f04898718355e9653c382b2`.

No annotated tag or remote branch is created by an agent. The user performs
all remote writes after reviewing this record.

## Mandatory release evidence

For the exact candidate ref, retain and verify:

1. strict GCC and Clang C99 warning-clean builds, registration/probe audit,
   ASan/UBSan, complete unit tests, examples, and `R CMD check --as-cran`,
   plus the separate explicit `--use-C23` installation and complete
   native-probe gate under repository-local GCC >= 15 and recent Clang;
2. real R 3.6.3, 4.0.5, 4.1.3, 4.2.3, 4.3.3, 4.4.3, and 4.5.2 runtime
   stages, with current R 4.6.1 complete execution owned by the full native
   lane;
   compilation against all seven pinned R 3.6.0--4.6.1 header axes; and the
   exact raw-attribute/old-R-closure-snapshot/stored-binding/promise exception
   ledger, raw-token/version-gated DSO audit, and option-access symbol policy.
   This includes the authenticated R-3.6-through-R-4.2 complete-test
   source-package closures, the R-4.3 data.table 1.18.4 overlay, the separate
   exact R-3.6 declared-floor smoke with `R_DEFAULT_PACKAGES` isolated, and
   the full-only sealed R-4.0.5-to-R-3.6.3 serialization handoff;
3. normalized Paradox-1 differential with reviewed intentional 2.0 deltas;
4. all ten exact reviewed overlay heads in the `release-refresh-20260720`
   profile, with all eight changed PR heads built and checked against both
   Paradox majors, then priority-zero/one reverse dependencies and maintained
   mlr-org repositories;
5. GCT, instrumented-R Valgrind, bounded rchk, direct routine/hazard probes,
   and adversarial corrupt-capsule/graph/ALTREP cases, treating hostile
   state-changing custom ALTREP as a safety/no-replay gate rather than an exact
   representation-equivalence gate;
6. package manuals/vignettes, active book/gallery/website/cheatsheets, and
   pure/recursive/default/opt-in/owner-bridge legacy serialized configuration
   upgrades;
7. GitHub current Windows x86-64, exact Windows x86-64 R 3.6.3/Rtools35, and
   macOS Apple-silicon ARM64 checks whose failure status is correctly
   propagated and whose exact source provenance is retained;
8. representative paired benchmarks on an idle host, including downstream
   call patterns, with raw distributions and regression thresholds reviewed.

Every accepted row receives a unique run ID, exact source ref/commit/tree,
commands, versions, logs, manifests, and completion seal. A transferred row
retains its donor execution identity and additionally names the target and exact
equivalence proof; verifier-only changes never relabel old execution as a new
package run.

### Historical provisional R 3.6 release-core evidence

The exact immutable ref `refs/paradox-release/r36-39855c9`, commit
`39855c919beec323fd5940e4c46286e8df1be8ff`, tree
`48e347b0c31a0f7ea966458586d13c1ab9d82764`, passed all eight tasks in
`.local/verify/runs/r36-release-39855c9`. This included strict native,
seven-axis header/API, differential, all four supported runtimes, declared
dependency floors, old-R stress, and the cross-runtime serialization handoff.
The coordinator completion, summary, native completion, API/header completion,
runtime top seal, and differential seal SHA-256 values are
`4a5d240f88ca5ed7593e0322e38749d38c515e02f5cb951b809d72d780225847`,
`8b5b7026d75dd7718d52ad7af7dbb5025e45e5a0bf26635714f3c6e933778ecd`,
`e12a74029b671db15bd5005b11aa4c243e7635e4d7efd67a1dd7eebe5e689cb6`,
`a9deab9d413adf7a0063b8665bac9df074ec324a535187cbd70df13be78415c2`,
`baf2e8462528d2814a06557b15d5f250cda7551dc84c7c2395210f870bbde58b`,
and
`18883e5b93c7625dd6d17710e23b00907e453f73e882f32c179e173d7d6c87b7`.
`release-core` excludes the separate combined memory gate. More importantly,
the subsequent graph-frame lifetime repair changes native package source, so
no row in this otherwise successful run transfers to the replacement
candidate.

### Historical retained local release evidence for `8797f11`

| Gate | Retained evidence and result |
|---|---|
| Full native, sanitizer, tests, and package checks | `.local/checks/serialized-migration-release-8797f11-20260724`; completion `0f1a0543ed0226ad956ec7a29114be7132f0de5945bcf2fd1a433ca7b325a998`; complete suite, CRAN-style and depends-only checks, strict compilers, analyzers, and sanitizers pass |
| R API/header matrix | `.local/checks/serialized-migration-release-8797f11-r-api-20260724`; completion `b979909684d10434056241670a241ba113427af5284f60155b0f523043116b80`; four R versions, 35 translation units, and two compilers pass |
| R 4.3.3 and 4.5.2 runtimes | `.local/checks/serialized-migration-release-8797f11-runtime-20260724/runtime-matrix`; top completion seal `33d0869e2af7416594a85c5e3cda500b6785ca169431b4e8e1adf35c99cea193`; each runtime passes 88 files and 6,227 expectations with 18 expected skips |
| Combined memory/adversarial | `.local/checks/serialized-migration-release-validation-a05cd51-memory-final-r1-20260724`; completion `a07df9509c78976d6d8a31a9b4f3415787f512e73bf85a2311d6ced672e9b226`; GCT, Valgrind, and bounded rchk pass, with the raw-versus-semantic analyzer identities recorded above |
| Exact Paradox-1 differential | `.local/compat/differential/runs/20260724T152627Z-3659648`; manifest `15fc85a7cf2236dd4f17eb8d36ab63c45e2920ea37d10f348825842e33e513e9`, seal `1055758ff59aee28ab9c67e3e35f0f8dfda3baadcace6a274abf0b78c71dad66`; 26 cases, three equal, 23 reviewed expected differences, zero unexpected |
| Paradox-2 exact source-package checks | `.local/compat/runs/migration-release-final-p2-8797f11-fc92edd-bench-r1/repository-checks-release-refresh-20260720-paradox2`; independently verified, five rows, zero failures, five final `Status: OK`; completion/results/manifest/seal `a43064cd48b78fd433b4a4995a2da9cfad422da7b3b66f53391de05fbd289179` / `f443e9ace27450057e4c8fcb6f9d93da85d595028abfff133272cc32f11a364c` / `aa6d0ad380f0453db8c87888bdd5b7d18d54bc2698d21b9a4e67ce1040bcd731` / `7de4719e3fa0016fb05d804aa240dea34a0e5de76763c7f1d5d885e2e581acf1` |
| Paradox-1 source-package compatibility | Exact final bbotk/mlr3mbo/celecx heads and mlr3fda base `8f5a3df` pass in `.local/compat/runs/migration-release-final-p1-cdcc8e6-221c95e-r2/repository-checks-release-refresh-20260720-paradox1` (manifest/seal `e224bc9a13043d6af6ad3715f3c9cc285e3095f2d53b7389094f90dff59bb0bc` / `c0ddb3e7de1d73a890b845f7cb19e6ff61cb5fb1e12eada0d868a6e34c54cfba`); final mlr3fda `c1cdad5` changes only the Paradox-2 snapshot file. The exact final miesmuschel pass is retained in `.local/compat/runs/migration-release-final-p1-cdcc8e6-2771f5d-r3/repository-checks-release-refresh-20260720-paradox1` (manifest/seal `4f56dc540e92e82474a2341774533963614ea11f43dfc4c7d11b7e89a7d41d57` / `b21a13cd65f08ba2094deddd5fd70876ca5573c92722f0fb972868107454860d`). The r2 stage itself is `completed_with_failures` because its obsolete miesmuschel row failed. |
| Scoped broad repository corpus | `.local/compat/runs/migration-release-final-p2-8797f11-fc92edd-bench-r1/repository-tests-priority-1-release-refresh-20260720-paradox2`; `jobs = 2`, 14 waves, 20 of 28 exact repositories green; completion/manifest/seal `cd0de96b8617d37d1f5e8e88ebb39ea34e02fae2378a5d198d215da813c6703e` / `376ed637f497a0980599396c6f1fffa52e087b68794e4cce765af1471c310c41` / `611ed1b2745143b34910f56ed1d6a3ce4f14031728955e1d4fe9d6cf389d4b9c` |
| Documentation | `.local/compat/runs/migration-release-final-p2-8797f11-fc92edd-bench-r1-documentation/documentation`; 17 workloads and every mandatory row pass; manifest/seal `dbd01bf40a1e6c776e8d82ea7d93b3494622b3c28b6fb9e21635257dc12ff528` / `0b2f5cf1df7dd386c34660daf8090fa6dbf135a7bba278ddb5ad85df5c49fef7` |
| Sealed release benchmark | `.local/benchmarks/serialized-migration-release-8797f11-sealed-r5-fc92edd-20260724`; 77 policy/decision rows (68 workloads plus nine consumer operations), 73 pass, four bounded marginal reviews, zero failures; completion/manifest/seal `5d615637bd6448bb95b8ba798cc8f7e23d40a78e1a63a4e3988eb436914109a0` / `a16d403c20ed8ab80233a877e81ec146b4a5c128fdf0a765be440993eb325499` / `d7a4bbefdba3e1d0a800b2e6bdae6e10ddde22b612cf05bebc5d019eba198fc7` |
| Windows/macOS portability | Direct-child companion `refs/paradox-release/portability-harness-5305ead`, commit `5305eaddbc9c2159fe194e6be10388c17b4c506a`, tree `e1fe00ddad08af89566e3df12460bc47d3e98292`; sole workflow SHA-256 `14c4c8d1cc8d8e07aea1829d1203f6217464ae9c6b94efc1050bf192638196e3`; hosted Windows x86-64/macOS ARM64 execution and retained artifacts pending |

The eight scoped broad-corpus exclusions are `mlr3tuningspaces`, `mlr3cluster`,
`mlr3filters`, `mlr3torch`, `xplainfi`, `mlr3extralearners`, `mlr3forecast`,
and `mlr3resampling`. Their retained logs diagnose upstream API drift,
optional-runtime or external-system absence, package-local defects, or bounded
environmental timeouts rather than a Paradox failure. The documentation stage's
nonrequired `mlr3book` full render is excluded for unrelated
`mlr3fairness::MeasureFairness` API drift; two legacy `mlr3gallery` rows are
excluded for the absent `distill` dependency. Focused Paradox book, website,
cheatsheet, `mbo_config`, and target rows pass.

### Historical retained local release evidence (superseded payload)

| Gate | Retained evidence and result |
|---|---|
| Package-payload equivalence | `.local/checks/package-equivalence-a461-10c6`; evidence manifest `e558864a318a465edf058013f743c6ae386b34bcd7894037a02c66938a986fb3`, completion `71de496c80ef836b3f3a8ce32a59883adf7c847b41eaa39dc05dc31dd8262a65`, normalized payload `e6e767b8fa3cd1a9273d62039c208d7c3ae1aea0b12d7446560297d4750dcc3e` |
| Exact candidate static/native source | `.local/checks/release-final-20260720T053518Z-10c6a0e-native-static`; completion `b42b77a87c210f5e2318bb0b25ec03abc16d220c5a99ab674c12ce2cfcde3b05` |
| Full native, sanitizer, tests, and package check | donor `.local/checks/release-final-20260720T022410Z-a4617ca-native`; completion `2b873adcc505de6e9cd7a4f08a61806bcff83417d1afc9cb901f972eeb95356f`; transferred only through the package-payload proof above |
| R API/header matrix | donor `.local/checks/release-final-20260720T022410Z-a4617ca-r-api`; completion `17e068591a3c7b59766ee5c41a55bd81c64830546097b8411a444334b1762725`; transferred only through the package-payload proof |
| R 4.3.3 and 4.5.2 runtimes | donor `.local/checks/release-final-20260720T022410Z-a4617ca-runtime`; top seal `2f396214bf4e3d53751b6306ac99531a71ee1e0e77fc4b7b3366f36dd546b235`; 84 files and 5,720 expectations on each runtime; transferred only through the package-payload proof |
| Exact candidate memory/adversarial | `.local/checks/release-final-20260720T053518Z-10c6a0e-memory`; completion `3220ee02820baf7e4ba9dfe90dacd79132f044bf088d045e4d8fb70036003d15`; Gctorture, Valgrind, and bounded rchk pass |
| Exact differential | `.local/compat/differential/runs/20260720T072548Z-375776`; manifest `460869a0511691eb4684dbb7bcc5a5a437b6e4477d95932892cbb808df27a9fd`, seal `1c485dfcad2e92a760e910a9ca805c5af98a2b55478689ff487284d07fe245c4`; maintained baseline `06091b5b64a78807d332ec95c5cdc1aaac5899b9`, 26 cases, 23 reviewed differences, zero unexpected |
| Exact payload priority consumers | donor `.local/compat/runs/release-final-20260720T022410Z-a4617ca-r3/repository-tests-priority-1-release-refresh-20260720-paradox2`; seven of seven passed, manifest/seal `e5dbb7ed0bf760c2082189aa9428500db05eeba787fd11ba99afaa03165db383` / `4adc9a9f1d0afd7ad515ccc939143ecf173f6fb3f4218f05ac72cf0d9a9860fa`; transferred through the package-payload proof, with final changed-profile rows below |
| Paradox-2 downstream profile | final r8 overlay manifest/seal `32f337eff8e2fe01c6e2575f5a45d631dad1ca3afb8b2c81d037ccb51a4f49be` / `d91de3224009f270ea69fadee13e7a2fdd19e4fc3c4eb2bf26ffc83a8b840fb7`; final miesmuschel suite `b781723ef6935fe2491dba02483c45eb8eb5d4eb34330ccd7d11044de3ffe34a` and check `6b93a0b7de555673aa5489c454a5ce62d267653a747f999e9eb4539b80716d3f`; unchanged three-head full-check manifest/seal from r6 `d1004b19ccdd5ef0ff9198b1b3ef1491aeaefc75c73e99ed19c166e34fd66ae4` / `a47586f5f4837fdd0c8a6583a72b86d54a52d1ab683afd39e6866f7ab51ea446` |
| Paradox-1 downstream profile | final r2 overlay manifest/seal `e48b62a5eb7bf16439cf9bb270965d26c1a8450cd622072a6f85d1124d1982d8` / `2885e9e01b71cc9d2d1276837dfe0fe4ff309b2a8a8269c70bf9dc8903be20ce`; final miesmuschel suite `f6c36c961fe1ebeeafda59b1c99ebc60f857a9f5e0ea06799e17ca65e427f991` and check `8cf57abb41df4743bc14520acbedcd8c9c04cefa6984250e1bde54a8c42209a5`; unchanged mlr3mbo/celecx/mlr3fda passed rows remain in the sealed r1 completed-with-failures stage |
| Documentation | `.local/compat/runs/release-final-20260720T053518Z-10c6a0e-r7-documentation/documentation`; manifest `142079a9a64525c3efbad3ce4f5b04a966581173c00a7ccbffe57109ea947dbe`, seal `ad3dfab8db699c66e7d013f00dca88f41d72fce5859180cc767d1f1d363046f2`; all 17 workloads completed and all mandatory rows pass |
| Release benchmark | `.local/benchmarks/release-final-20260720T053518Z-10c6a0e-r7-release`; manifest `ee70b0bc9ca2b66710e56642c3641a1deb02816dba208e6851957b58ce8d938e`, seal `be525645f72ac8fa43f3f4064a0d00ba4ae1b69b8826441ca913e104b66a70e3`; 72 pass, five bounded marginal reviews, zero failures |
| Windows/macOS portability | local harness fixtures pass; exact remote run for candidate tag `paradox-2.0.0-ci-10c6a0e` remains pending |

## Release decision

The release decision remains open for active candidate `a0a9ff3`, but every
applicable local gate is complete. Its exact compiler/runtime/package and
memory foundation, fresh dual-axis compatibility and documentation results,
reviewed factual broad/reverse aggregates, exact-head checks, and independently
sealed benchmark are accepted. Hosted run `30853585319` completed every job
against direct child `da5a500`, but its CRLF-converted retained runtime lock
failed the offline byte-identity gate; it is rejected harness evidence and
must not be retried unchanged. Replacement direct-child companion `00a24cb`
passes local validation, and hosted run `30941929181` plus retained offline
evidence are accepted. Hosted portability is complete; only manual downstream
and publication coordination remains. That work includes
mark-ready/review/merge/release sequencing for the eight downstream PRs,
dependency release ordering, the release tag, and final release coordination.

## Historical rejected or superseded refs

Candidate `refs/paradox-release/candidate-20260731T150816Z` at `a153fae`
closed its exact bounded Domain comparison but is rejected as a release
candidate by `release-candidate-a153fae`. Its five passing and three failing
tasks are recorded in the status section. The run stopped the dependent
every-minor matrix before execution and produced no reusable native source
donor; the C23 precompile harness failure and the formal-S4/legacy-table defects
must be proved anew against a replacement ref.

The compatibility-first candidate at
`refs/paradox-release/candidate-20260717T083921Z`, commit
`2f40e3e567c6d4fa568384622cb4e2d81c3fb2fa`, once satisfied a different
contract that preserved private surfaces and dual engines. The later
contract-first development candidate at
`refs/paradox-release/candidate-20260719T104709Z`, commit
`5e40d2ba9b9ce3a75615b90420fb9bc298c19ecf`, and its direct-child portability
companion `refs/paradox-release/portability-harness-268ccff` at
`268ccff27ee68bfea71c6370b0616a9c969a94cf` predate the final public-subset,
bulk-dependency, downstream-bridge, and `$has_deps` changes. All of these refs,
hashes, logs, and artifacts are historical only. They authorize no conclusion
about current package bytes and must not be copied into the candidate fields
above.

The still later candidate
`refs/paradox-release/candidate-20260719T150831Z`, commit
`612345ceb403c70a0ea6c1149c367c6782d9870b`, passed the native, R-API,
runtime, differential, and memory gates recorded for its exact bytes. Its final
release benchmark then found a release-blocking correctness defect before any
candidate timing: R 4.6 exposed the benchmark's ordinary wide data.frame as a
top-level base `wrap_list` ALTREP, which `check_dt()` rejected. The benchmark
remains deliberately unsealed. This candidate and every package-byte-bound
result for it are superseded; the retained evidence explains the replacement
public-table snapshot boundary but cannot be promoted to the new candidate.

The next candidate,
`refs/paradox-release/candidate-20260719T175053Z`, commit
`4f28327f894fe17324410a45cabaf7221e6eca45`, passed its exact-byte native,
R-API, runtime, differential, and downstream-bridge gates. Its memory run was
stopped and deliberately left unsealed after adversarial review found two
release blockers in the shared public-table path: missing, S4, and
dimension-mismatched `row.names` were not consistently rejected, and a hostile
top-shell Elt callback could mutate a still-shared names vector with
`data.table::setnames()`. This candidate is superseded. No completed or partial
package-byte evidence from it transfers to a replacement candidate; the
unsealed memory run is retained only as diagnostic history.

The following candidate,
`refs/paradox-release/candidate-20260719T194741Z`, commit
`60704fcc6a899c508f5adffbec35bb61723d3704`, installed successfully and built
the exact downstream bridge overlay, but failed its R-API gate before release
convergence. Strict Clang against the pinned R 4.3 and 4.4 headers rejected two
implicit signedness conversions from the `Rboolean` result of `Rf_isObject()`
to `int`. Native and runtime work already in progress was stopped; all partial
or completed package-byte evidence for this candidate is superseded and may
not be promoted. The fix uses explicit truth comparisons and requires a new
candidate with a complete fresh gate set.

Candidate `refs/paradox-release/candidate-20260719T200549Z`, commit
`70c6d728785464c98ffc8c658f20c1937467a593`, passed the replacement R-API gate
and built its exact package and bridge bytes. The R 4.3 runtime stage then
rejected an internal test which assumed that reinstalling an attribute on a
wide data.table always creates base R's top-level `wrap_list` ALTREP. R 4.3
left that input ordinary, for which the materializer correctly returns the
table unchanged; only an admitted ALTREP shell is normalized and stripped of
ignored caches. The fix makes the cache-disposal assertion use the portable
native ALTREP fixture and retains the base wrapper as conditional realistic
coverage. Expanding production copying to ordinary tables was rejected as an
unnecessary hot-path cost. The partial native/runtime results and completed
API/package/bridge evidence are all superseded.

Candidate `refs/paradox-release/candidate-20260719T202524Z`, commit
`e3741ab1d3cb8a5f3e7f357af6b7ab90c6e50fb7`, passed its exact-byte native,
R-API, and runtime gates, then failed the maintained repository sweep in
mlr3fselect. `mlr3::BenchmarkResult$aggregate()` produces
the ordinary additive class vector
`c("bmr_aggregate", "data.table", "data.frame")`, and data.table 1.18.4
deliberately preserves that class through a narrow `with = FALSE` subset before
bbotk calls `ParamSet$assert_dt()`. The exact-class classifier rejected this
non-dispatching representation even though Paradox 1 accepted it. The
replacement contract admits well-formed additive leading classes, drops them
from an already-required ALTREP snapshot while avoiding any prefix-only copy of
ordinary input, and retains the strict table attribute/cache boundary.
All evidence bound to `e3741ab` is superseded and requires a fresh candidate.

Candidate `refs/paradox-release/candidate-20260720T022410Z`, commit
`a4617ca769ff5373a7da16c7ce333e36c68fd9b2`, fixed that public-table boundary
and passed its exact R-API, native, runtime, differential, focused downstream,
and bounded-performance gates. Gctorture and every retained Valgrind diagnostic
inventory were clean. Its combined memory run remained unsealed because the
source-bound rchk policy still named the pre-classifier report hashes and
69-routine count. The actual bounded report preserved the reviewed 77 Function
blocks, 196 UP diagnostics, and 13 PB diagnostics; the classifier's three
diagnostic registrations raised the routine count to 72. The replacement
candidate incorporates that exact refreshed policy and final validation
tooling without changing package-facing files. The independently replayed
`.local/checks/package-equivalence-a461-10c6` proof establishes that all 19
changed Git paths are `.Rbuildignore`-excluded and that both clean builds have
the same 217-file package payload after removing only R's generated `Packaged:`
record. Thus its completed R-API, full native/sanitizer/test/check, runtime, and
focused-consumer conclusions transfer to the identical `10c6a0e` package
payload while retaining their donor identity. Neither the partial `a4617ca`
memory directory nor its earlier tooling-bound overlays transfer; exact
`10c6a0e` static, memory, differential, documentation, downstream, and benchmark
evidence is retained separately above.
