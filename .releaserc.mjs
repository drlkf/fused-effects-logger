/**
 * @type {import('semantic-release').GlobalConfig}
 */
export default {
    branches: ["main"],
    tagFormat: "${version}",
    plugins: [
        [
            "@semantic-release/commit-analyzer",
            {
                "preset": "conventionalcommits",
                "releaseRules": [
                    { "type": "docs", "release": "patch" }
                ]
            }
        ],
        "@semantic-release/release-notes-generator",
        [
            "semantic-release-mirror-version",
            {
                "fileGlob": "*(package.yaml|fused-effects-logger.cabal)",
                "placeholderRegExp": /(?<=\.)[0-9]+\.[0-9]+\.[0-9]+/g
            }
        ],
        [
            "@semantic-release/changelog",
            {
                changelogFile: "CHANGELOG.md",
                changelogTitle: "Changelog"
            }
        ],
        [
            "@semantic-release/git",
            {
                "assets": [
                    "package.yaml",
                    "fused-effects-logger.cabal",
                    "CHANGELOG.md",
                    "debian/changelog",
                ],
                "message": "chore(release): ${nextRelease.version}\n\n${nextRelease.notes}"
            }
        ]
    ]
};
