# Portuguese Translations (Português)

## Overview

Comprehensive Portuguese (pt) translations have been added to the Free Association project. This document provides an overview of all translation files and their coverage.

## Translation Files Created

### 1. **common.json** - Common UI Elements
Contains 80+ common UI strings including:
- Basic actions: save, cancel, delete, edit, close, back, next
- Data operations: export, import, copy, paste, cut, undo, redo
- Status messages: error, warning, info, success
- Form elements: required, optional, name, description
- Authentication: username, password, login, logout, register
- Navigation: home, menu, dashboard, settings

### 2. **home.json** - Home Page
- Welcome messages and subtitles
- Recognition system labels (Your Recognition, Mutual Recognition)
- View switcher labels (Tree, Map, Inventory)
- Empty state messages
- Bar chart labels and tooltips

### 3. **collective.json** - Collective Features
- Collective management: create, join, leave
- Member management
- Permissions and roles (admin, moderator, member)
- Settings and configuration

### 4. **auth.json** - Authentication
- Login/Register forms
- Password management (change, reset, confirm)
- Error messages for authentication
- Terms and privacy policy acceptance
- Public key management
- Session management

### 5. **inventory.json** - Inventory & Capacities
- Capacity management (create, edit, delete)
- Capacity properties (name, description, unit, quantity, price)
- Availability slots and scheduling
- Location types (physical, virtual, remote)
- Geographic data (coordinates, address, city, state, country)
- Provider and recipient roles
- Status filters (allocated, available, unavailable)
- Search and filter options

### 6. **map.json** - Map Features
- Map views (satellite, street, terrain, 3D)
- Location sharing and live tracking
- Navigation and directions
- Markers and layers
- Zoom and centering controls
- Location permissions and errors
- Search functionality

### 7. **tree.json** - Tree/Node System
- Node types (priority, value, goal, dependency, desire, contribution)
- Node operations (add, edit, delete, rename, move, copy, paste)
- Node properties (points, fulfillment, contributors)
- Navigation (expand, collapse, go to parent/root)
- Contributor management (add/remove contributors and anti-contributors)
- Templates (SDG, blank)
- Success/error messages for operations

### 8. **contacts.json** - Contact Management
- Contact operations (add, edit, delete)
- Contact properties (name, alias, ID, public key)
- Tags and favorites
- Status indicators (online, offline, last seen)
- Mutual contributors
- Search and filtering

### 9. **toolbar.json** - Toolbar & Navigation
- Search functionality
- View switcher
- Mode toggles (delete, recompose, text edit)
- Forest view and contributor selection
- Data import/export
- Help and guided tour
- Keyboard shortcuts
- Filter controls

### 10. **notifications.json** - Notifications
- Notification management
- Read/unread status
- Notification settings
- Permission handling
- Feature status messages

### 11. **errors.json** - Error Messages
- Generic errors
- Network and connection errors
- Validation errors
- Field validation (required, format, length, value ranges)
- Action suggestions (try again, reload, contact support)

## Total Coverage

- **Total translation keys**: 400+
- **Total translation files**: 11 files
- **Features covered**: 
  - Authentication & User Management
  - Tree/Node System
  - Inventory & Capacity Management
  - Map & Location Services
  - Contacts & Social Features
  - Collectives
  - Notifications
  - Error Handling
  - Common UI Elements

## Usage Examples

### Basic Usage
```svelte
<script>
  import { t } from '$lib/translations';
</script>

<h1>{$t('home.title')}</h1>
<button>{$t('common.save')}</button>
<p>{$t('auth.welcome_user', { name: userName })}</p>
```

### Authentication
```svelte
<h2>{$t('auth.login')}</h2>
<input placeholder={$t('auth.username')} />
<input type="password" placeholder={$t('auth.password')} />
<button>{$t('auth.sign_in')}</button>
<p>{$t('auth.dont_have_account')} <a>{$t('auth.create_one_now')}</a></p>
```

### Inventory Management
```svelte
<h2>{$t('inventory.capacities')}</h2>
<button>{$t('inventory.add_capacity')}</button>
<label>{$t('inventory.capacity_name')}</label>
<label>{$t('inventory.location_type')}</label>
<span>{$t('inventory.allocated_slots')}: {count}</span>
```

### Tree Operations
```svelte
<button>{$t('tree.add_child')}</button>
<button>{$t('tree.add_contributor')}</button>
<label>{$t('tree.node_fulfillment')}</label>
<p>{$t('tree.no_contributors')}</p>
```

### Map Features
```svelte
<h2>{$t('map.title')}</h2>
<button>{$t('map.share_location')}</button>
<button>{$t('map.my_location')}</button>
<select>
  <option>{$t('map.street_view')}</option>
  <option>{$t('map.satellite_view')}</option>
</select>
```

## Language Detection

The system automatically detects Portuguese browsers:
- Browser language starting with "pt" → Portuguese
- Browser language starting with "tr" → Turkish  
- All others → English (default)

Users can manually switch languages using the language switcher in the header.

## Testing

Visit `/i18n-test` to see all translations in action:
1. Open the app
2. Use the language switcher to select "Português"
3. Navigate through different pages to see translations
4. Test with different components (tree, map, inventory)

## Notes

- All dates and times should be formatted according to Portuguese locale (pt-BR or pt-PT)
- Currency values should use appropriate Portuguese formatting
- Number formatting follows Portuguese conventions (comma for decimal, period for thousands)
- The translations use neutral Portuguese that works for both Brazilian (pt-BR) and European (pt-PT) Portuguese

## Contributing

To add more translations:
1. Add new keys to the appropriate JSON file in `src/lib/translations/pt/`
2. Update the same keys in English (`en/`) and Turkish (`tr/`) files
3. Register new translation files in `src/lib/translations/index.ts` if creating a new file
4. Test the translations across different components

## Future Enhancements

Potential additions:
- Regional variants (Brazilian vs. European Portuguese)
- Date/time formatting utilities
- Currency formatting helpers
- Pluralization rules
- RTL support for future languages
- Translation completion tracking
- Missing translation fallbacks

