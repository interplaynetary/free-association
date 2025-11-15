# GitBook Setup Guide

This guide explains how to sync the `/docs` folder with GitBook for beautiful, accessible documentation.

## Method 1: GitBook GitHub Integration (Recommended)

This method automatically syncs your docs folder with GitBook whenever you push to GitHub.

### Setup Steps

**1. Create GitBook Account**
- Go to [gitbook.com](https://www.gitbook.com)
- Sign up (can use GitHub account)
- Create a new organization or use personal account

**2. Create New Space**
- Click "New Space"
- Choose "Import from GitHub"
- Authorize GitBook to access your GitHub account

**3. Configure Repository**
- Select repository: `free-association`
- Choose branch: `main` (or your preferred branch)
- Set root directory: `docs`
- GitBook will detect `SUMMARY.md` automatically

**4. Configure Sync**
- GitBook will create a `.gitbook.yaml` file (already created in `/docs`)
- Verify settings in GitBook dashboard
- Enable "Auto-sync on commit"

**5. Push and Sync**
```bash
git add docs/
git commit -m "Add GitBook documentation structure"
git push origin main
```

GitBook will automatically sync within a few minutes.

### Configuration

The `.gitbook.yaml` file in `/docs` configures:

```yaml
root: ./                    # Root of documentation
structure:
  readme: README.md         # Landing page
  summary: SUMMARY.md       # Table of contents
```

### Access Your Docs

**Published URL:** gitbook.com/your-org/your-space

**Custom Domain:** Configure in GitBook settings → Domain

---

## Method 2: GitBook CLI

For manual syncing or CI/CD integration.

### Installation

```bash
npm install -g gitbook-cli
```

### Setup

**1. Initialize GitBook**
```bash
cd docs
gitbook init
```

**2. Build Locally**
```bash
gitbook serve
# Opens at http://localhost:4000
```

**3. Build Static Site**
```bash
gitbook build
# Output in _book/ directory
```

### Publishing

**Manual:**
- Build locally
- Upload `_book/` contents to hosting

**CI/CD:**
- Add build step to GitHub Actions
- Deploy to GitHub Pages, Netlify, Vercel, etc.

---

## Method 3: Direct GitHub Pages

Use GitHub Pages with Jekyll or static site generator.

### GitHub Pages Setup

**1. Enable GitHub Pages**
- Repository Settings → Pages
- Source: Branch `main`, folder `/docs`
- Save

**2. Add Jekyll Config (Optional)**

Create `docs/_config.yml`:
```yaml
theme: jekyll-theme-minimal
title: Free Association
description: Digital Public Infrastructure for Resource Coordination
```

**3. Access**
- URL: `https://[username].github.io/free-association/`

---

## Recommended Workflow

**For Active Development:**

```bash
# 1. Make documentation changes
vim docs/concepts/how-it-works.md

# 2. Preview locally (optional)
cd docs && gitbook serve

# 3. Commit and push
git add docs/
git commit -m "Update how-it-works documentation"
git push origin main

# 4. GitBook auto-syncs within minutes
```

**For Major Updates:**

1. Create feature branch
2. Update documentation
3. Review locally with `gitbook serve`
4. Create pull request
5. Merge to main → GitBook syncs automatically

---

## GitBook Features

Once connected, GitBook provides:

**Collaboration:**
- Inline comments
- Change tracking
- Multiple editors
- Version history

**Publishing:**
- Public or private spaces
- Custom domains
- PDF/eBook export
- Search functionality

**Customization:**
- Themes and styling
- Logo and branding
- Navigation customization
- Analytics integration

---

## Maintaining Sync

**What Syncs Automatically:**
- All `.md` files in `/docs`
- `SUMMARY.md` structure
- Images and assets
- Directory organization

**What Requires Manual Update:**
- GitBook customization (themes, domain)
- Space settings
- Permissions and access

**Sync Frequency:**
- Automatic on every push to configured branch
- Typically syncs within 2-5 minutes
- View sync status in GitBook dashboard

---

## Troubleshooting

### Sync Not Working

**Check:**
1. GitHub integration still authorized
2. Branch name is correct
3. `/docs` path is configured
4. No GitBook-specific errors in dashboard

**Fix:**
- Re-authorize GitHub integration
- Manually trigger sync in GitBook dashboard
- Check GitBook build logs

### Links Not Working

**Relative Links:**
- Use relative paths: `[text](../folder/file.md)`
- GitBook handles conversion automatically

**Absolute Links:**
- Use full URLs for external links
- Keep internal links relative

### Images Not Showing

**Path Issues:**
- Store images in `/docs` subdirectories
- Use relative paths: `![alt](./images/diagram.png)`
- Ensure images committed to repository

---

## Next Steps

After setup:

1. ✅ Complete all documentation files (done)
2. ✅ Create `.gitbook.yaml` configuration (done)
3. ⬜ Create GitBook account
4. ⬜ Connect GitHub repository
5. ⬜ Configure custom domain (optional)
6. ⬜ Share documentation URL

---

## Resources

**GitBook Documentation:** [docs.gitbook.com](https://docs.gitbook.com)

**GitHub Integration:** [docs.gitbook.com/integrations/github](https://docs.gitbook.com/integrations/github)

**GitBook CLI:** [github.com/GitbookIO/gitbook](https://github.com/GitbookIO/gitbook)

**Support:** Your GitBook dashboard → Help

---

## Contact

Questions about GitBook setup?

**Project Contact:** info@openassociation.org

**GitBook Support:** support.gitbook.com

