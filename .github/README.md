# GitHub Actions Workflows

This directory contains automated workflows for maintaining the website and CV.

## Workflows

### 1. Quarto Publish (`quarto-publish.yml`)

**Purpose**: Automatically builds and deploys the Quarto website to GitHub Pages.

**Triggers**:
- Push to `main` branch
- Manual trigger via workflow_dispatch

**What it does**:
1. Sets up R environment and dependencies
2. Installs Quarto
3. Renders the website
4. Publishes to gh-pages branch

**Best Practices Implemented**:
- Concurrency control prevents simultaneous deployments
- 20-minute timeout prevents hanging jobs
- Can be manually triggered if needed

---

### 2. Monthly CV Update (`update-cv.yml`)

**Purpose**: Automatically updates CV with latest publications from ORCID.

**Triggers**:
- Scheduled: 1st day of every month at midnight UTC
- Manual trigger via workflow_dispatch (important - see below!)

**What it does**:
1. Fetches latest publications from ORCID
2. Updates `research/index.qmd` with publication list
3. Updates `cv/resume.typ` with publications
4. Compiles the CV to PDF using Typst
5. Commits and pushes changes back to repository

**Best Practices Implemented**:
- Concurrency control prevents overlapping update runs
- 30-minute timeout prevents hanging jobs
- Error handling for missing ORCID_TOKEN
- Retry logic for git push operations
- Uses `[skip ci]` to prevent trigger loops

**Requirements**:
- `ORCID_TOKEN` must be set in repository secrets

---

## Important: GitHub's 60-Day Workflow Disable Policy

⚠️ **CRITICAL INFORMATION ABOUT SCHEDULED WORKFLOWS**

GitHub automatically **disables scheduled workflows** (like the Monthly CV Update) if:
- There is **no repository activity for 60 days**
- This includes no commits, no pull requests, no issues, etc.

### Why This Happens

When a repository is inactive for 60 days, GitHub assumes the project is dormant and disables all scheduled workflows to save resources. You'll receive an email notification when this occurs:

> "The 'Monthly CV Update' workflow in oddish3/oddish.github.io has been disabled"

### How to Prevent Auto-Disable

You have several options:

1. **Manual Trigger (Recommended)**:
   - Visit: Actions → Monthly CV Update → Run workflow
   - This counts as repository activity and resets the 60-day timer
   - Run this every ~2 months if you're not actively committing

2. **Make Regular Commits**:
   - Any commit to the repository resets the timer
   - The workflow itself commits changes when publications update

3. **Keep-Alive Workflow** (Not Implemented):
   - Could add a separate workflow that commits a timestamp file every 50 days
   - Decided against this to avoid polluting git history

### How to Re-Enable After Disable

If the workflow gets disabled:

1. Go to the **Actions** tab in your repository
2. Click on "**Monthly CV Update**" in the left sidebar
3. Click the "**Enable workflow**" button
4. Optionally, click "**Run workflow**" to trigger it immediately

### Monitoring

- Check your email for GitHub notifications
- Visit the Actions tab periodically to verify workflows are enabled
- The workflow_dispatch trigger ensures you can always run it manually

---

## Secrets Required

The following secrets must be configured in repository settings:

1. **ORCID_TOKEN**: Required for the Monthly CV Update workflow
   - Used to authenticate with ORCID API
   - Get from: https://orcid.org/developer-tools
   - Add via: Settings → Secrets and variables → Actions → New repository secret

2. **GITHUB_TOKEN**: Automatically provided by GitHub
   - Used for pushing commits and deploying pages
   - No configuration needed

---

## Manual Workflow Execution

Both workflows can be manually triggered:

1. Go to the **Actions** tab
2. Select the workflow you want to run
3. Click "**Run workflow**"
4. Select the branch (usually `main`)
5. Click "**Run workflow**" button

This is useful for:
- Testing changes to workflows
- Forcing an update outside the schedule
- Re-running after fixing an error
- Resetting the 60-day inactivity timer

---

## Troubleshooting

### Workflow Fails with "ORCID_TOKEN secret is not set"

**Solution**: Add the ORCID_TOKEN secret:
1. Go to Settings → Secrets and variables → Actions
2. Click "New repository secret"
3. Name: `ORCID_TOKEN`
4. Value: Your ORCID API token
5. Click "Add secret"

### Workflow is Disabled

**Cause**: No repository activity for 60 days

**Solution**: See "How to Re-Enable After Disable" section above

### Git Push Fails in CV Update

**Cause**: Concurrent workflow runs or external commits

**Solution**: The workflow includes retry logic. If it persists:
1. Check for concurrent workflow runs
2. Manually pull latest changes
3. Re-run the workflow

### Publications Not Updating

**Possible causes**:
1. ORCID profile not updated
2. DOIs not in ORCID record
3. Crossref API issues

**Debug**:
1. Check workflow logs in Actions tab
2. Verify publications are in your ORCID profile
3. Ensure DOIs are correct format

---

## Maintenance

### Updating Dependencies

When updating action versions in workflows:

1. Check the action's repository for breaking changes
2. Test in a separate branch first
3. Update version numbers gradually (don't skip major versions)

### Modifying the Schedule

To change the CV update frequency, edit the cron expression in `update-cv.yml`:

```yaml
schedule:
  - cron: '0 0 1 * *'  # Current: Monthly on the 1st
  # Examples:
  # - cron: '0 0 * * 0'  # Weekly on Sunday
  # - cron: '0 0 1 */3 *'  # Quarterly (every 3 months)
```

**Note**: More frequent schedules use more GitHub Actions minutes but help prevent the 60-day disable.

---

## Additional Notes

- All workflows use Ubuntu latest runners
- R version is set to 'release' for stability
- Quarto uses the latest version from quarto-dev
- Typst is installed via the official setup action
- Font Awesome is required for CV compilation
