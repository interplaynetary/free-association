# Portuguese Translations - Quick Reference

## 🎯 Quick Facts

- **Language Code**: `pt`
- **Language Name**: Português
- **Translation Files**: 11
- **Total Keys**: 400+
- **Status**: ✅ Complete

## 📂 File Structure

```
src/lib/translations/pt/
├── common.json       (80+ keys) - UI basics, buttons, forms
├── home.json         (15 keys)  - Home page, recognition system
├── collective.json   (15 keys)  - Collective management
├── auth.json         (50+ keys) - Authentication, passwords
├── inventory.json    (60+ keys) - Capacities, shares, slots
├── map.json          (30+ keys) - Map features, location
├── tree.json         (60+ keys) - Nodes, contributors, navigation
├── contacts.json     (25+ keys) - Contact management
├── toolbar.json      (40+ keys) - Search, views, modes
├── notifications.json (10+ keys) - Notifications
└── errors.json       (25+ keys) - Error messages
```

## 🔑 Most Common Translations

### Actions
```
save          → Salvar
cancel        → Cancelar
delete        → Excluir
edit          → Editar
add           → Adicionar
create        → Criar
search        → Buscar
close         → Fechar
back          → Voltar
```

### Authentication
```
login         → Entrar
logout        → Sair
sign_in       → Entrar
sign_up       → Cadastrar
username      → Nome de usuário
password      → Senha
register      → Registrar
```

### Status
```
loading       → Carregando...
success       → Sucesso
error         → Erro
warning       → Aviso
```

### Tree/Node
```
priority      → Prioridade
goal          → Objetivo
contribution  → Contribuição
add_child     → Adicionar Filho
contributors  → Contribuidores
```

### Inventory
```
capacity      → Capacidade
share         → Compartilhar
available     → Disponível
allocated     → Alocado
provider      → Provedor
```

## 💻 Usage in Code

### Basic
```svelte
{$t('common.save')}
{$t('auth.login')}
{$t('tree.add_child')}
```

### With Variables
```svelte
{$t('auth.welcome_user', { name: userName })}
{$t('toolbar.navigated_to', { name: nodeName })}
```

### Conditional
```svelte
{#if isLoading}
  {$t('common.loading')}
{:else}
  {$t('common.save')}
{/if}
```

### Form Labels
```svelte
<label>{$t('inventory.capacity_name')}</label>
<input placeholder={$t('auth.username')} />
<button>{$t('common.submit')}</button>
```

## 🌍 Language Selection

### Auto-Detection
Browser language `pt-*` → Portuguese automatically

### Manual Selection
Header → Language Switcher → "Português"

### Programmatic
```javascript
import { setLocale } from '$lib/translations';
setLocale('pt');
```

## 🧪 Testing Checklist

- [ ] Select Portuguese from language switcher
- [ ] Check home page translations
- [ ] Test login/register forms
- [ ] Verify tree node labels
- [ ] Check inventory/capacity pages
- [ ] Test map features
- [ ] Verify contact management
- [ ] Check toolbar buttons
- [ ] Test error messages
- [ ] Verify toast notifications

## 📱 Supported Features

✅ Authentication flows  
✅ Tree navigation  
✅ Node management  
✅ Capacity creation/editing  
✅ Share viewing  
✅ Map controls  
✅ Contact management  
✅ Collective features  
✅ Search functionality  
✅ Error handling  
✅ Notifications  
✅ Form validation  

## 🆘 Troubleshooting

### Translations not showing?
1. Check language switcher selected "Português"
2. Clear localStorage: `localStorage.removeItem('lang')`
3. Reload page
4. Check browser console for errors

### Missing translation?
1. Check if key exists in `/src/lib/translations/pt/*.json`
2. Verify loader registered in `index.ts`
3. Check route-specific loaders

### Wrong language detected?
1. Check `navigator.language` in console
2. Manually select from language switcher
3. Check localStorage: `localStorage.getItem('lang')`

## 📞 Key Translation Categories

| Category | File | Keys | Purpose |
|----------|------|------|---------|
| UI Basics | common.json | 80+ | Buttons, forms, actions |
| Home | home.json | 15 | Home page, recognition |
| Auth | auth.json | 50+ | Login, register, passwords |
| Tree | tree.json | 60+ | Nodes, contributors |
| Inventory | inventory.json | 60+ | Capacities, shares |
| Map | map.json | 30+ | Location, navigation |
| Contacts | contacts.json | 25+ | Contact management |
| Toolbar | toolbar.json | 40+ | Views, search, modes |
| Collective | collective.json | 15 | Group management |
| Notifications | notifications.json | 10+ | Alerts, settings |
| Errors | errors.json | 25+ | Error messages |

## 🔗 Related Documentation

- **I18N_GUIDE.md** - Complete internationalization guide
- **PORTUGUESE_TRANSLATIONS.md** - Detailed translation overview
- **PORTUGUESE_SETUP_SUMMARY.md** - Implementation details

## ✨ Quick Commands

```bash
# Start dev server
bun run dev

# Check types
bun run check

# Build for production
bun run build

# Test i18n page
# Navigate to: http://localhost:5173/i18n-test
```

---

**Ready to use!** 🎉 Portuguese is fully configured and ready for production.

