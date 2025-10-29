# VoiceInput Component

A Svelte 5 component that provides voice-to-text transcription using the Web Speech API.

## Features

- 🎤 **Start/Stop Recording** - Control when to capture speech
- ⏸️ **Pause/Resume** - Pause recording and resume later
- 🗑️ **Clear** - Clear the transcript
- 📝 **Live Transcription** - See interim results as you speak (displayed in italics)
- ✏️ **Editable** - Manually edit the transcribed text
- 🌍 **Multi-language Support** - Supports different languages
- 🎨 **Consistent Styling** - Matches application design patterns
- 📱 **Mobile Responsive** - Works on mobile devices
- ⚠️ **Error Handling** - Graceful error messages for common issues
- 🔒 **Permission Handling** - Handles microphone permission requests

## Browser Compatibility

The Web Speech API is supported in:

- Chrome (version 25+)
- Edge (version 79+)
- Safari (version 14.1+)
- Opera (version 27+)

**Note:** Firefox has limited support. For best results, use Chrome, Edge, or Safari.

## Usage

### Basic Usage

```svelte
<script>
  import VoiceInput from '$lib/components/VoiceInput.svelte';
  
  let transcript = $state('');
</script>

<VoiceInput bind:value={transcript} />
```

### With Event Handling

```svelte
<script>
  import VoiceInput from '$lib/components/VoiceInput.svelte';
  
  let transcript = $state('');
  
  function handleChange(newTranscript: string) {
    console.log('New transcript:', newTranscript);
    // Do something with the transcript
  }
</script>

<VoiceInput
  bind:value={transcript}
  onchange={handleChange}
/>
```

### Custom Configuration

```svelte
<script>
  import VoiceInput from '$lib/components/VoiceInput.svelte';
  
  let transcript = $state('');
</script>

<VoiceInput
  bind:value={transcript}
  placeholder="Start speaking..."
  lang="es-ES"
  continuous={true}
  interimResults={true}
  editable={true}
  onchange={(text) => console.log(text)}
/>
```

### Read-Only Mode

```svelte
<VoiceInput
  placeholder="This is read-only"
  editable={false}
/>
```

## Props

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `value` | `string` | `''` | Bindable transcript value |
| `placeholder` | `string` | `'Click the microphone to start recording...'` | Placeholder text when no transcription |
| `lang` | `string` | `'en-US'` | Language code for speech recognition (e.g., 'es-ES', 'fr-FR', 'de-DE') |
| `continuous` | `boolean` | `true` | Enable continuous recognition (keeps listening) |
| `interimResults` | `boolean` | `true` | Show interim results while speaking |
| `maxAlternatives` | `number` | `1` | Maximum number of alternative transcriptions |
| `onchange` | `(transcript: string) => void` | `undefined` | Callback when transcript changes |
| `editable` | `boolean` | `true` | Allow manual editing of the transcript |

## Language Codes

Common language codes you can use:

- `'en-US'` - English (United States)
- `'en-GB'` - English (United Kingdom)
- `'es-ES'` - Spanish (Spain)
- `'es-MX'` - Spanish (Mexico)
- `'fr-FR'` - French (France)
- `'de-DE'` - German (Germany)
- `'it-IT'` - Italian (Italy)
- `'pt-BR'` - Portuguese (Brazil)
- `'ru-RU'` - Russian
- `'zh-CN'` - Chinese (Simplified)
- `'ja-JP'` - Japanese
- `'ko-KR'` - Korean
- `'ar-SA'` - Arabic (Saudi Arabia)
- `'hi-IN'` - Hindi (India)

For a complete list, see [BCP 47 language tags](https://www.rfc-editor.org/rfc/bcp/bcp47.txt).

## States

The component has three main states:

1. **Idle** - Not recording, ready to start
2. **Recording** - Actively capturing speech
3. **Paused** - Recording paused, can resume

## Error Handling

The component handles various errors gracefully:

- `no-speech` - No speech detected (auto-clears after 5 seconds)
- `audio-capture` - No microphone found
- `not-allowed` - Microphone access denied
- `network` - Network error (auto-clears after 5 seconds)
- Other errors display generic error messages

## Styling

The component uses a consistent design system:

- **Primary Button (Start)** - Blue (#3b82f6)
- **Warning Button (Pause)** - Amber (#f59e0b)
- **Success Button (Resume)** - Green (#10b981)
- **Secondary Button (Stop)** - Gray (#6b7280)
- **Danger Button (Clear)** - Red (#ef4444)

All buttons have hover effects and are mobile-responsive.

## Accessibility

- All buttons have `title` attributes for tooltips
- Proper ARIA labels and roles
- Keyboard-accessible controls
- Clear visual feedback for recording state
- Error messages are clearly displayed

## Mobile Considerations

- Touch-friendly button sizes
- Responsive layout that adapts to small screens
- Icon-only buttons on mobile to save space
- Proper touch action handling to prevent zoom

## Security & Privacy

The component:

- Requests microphone permission before use
- Only captures audio when actively recording
- Stops recording when component is destroyed
- Does not store or transmit audio data (transcription happens locally in the browser)

## Demo

Visit `/voice-demo` in your application to see the component in action with:

- Editable and read-only examples
- Real-time transcript updates
- Event logging
- Usage examples
- Browser compatibility information

## Implementation Details

The component uses:

- `SpeechRecognition` or `webkitSpeechRecognition` (for Safari)
- Event-driven architecture for handling results and errors
- Svelte 5 runes (`$state`, `$derived`, `$bindable`)
- Automatic cleanup on component destroy

## Troubleshooting

### Network Error (Most Common Issue)

If you get a "network" error, this is usually due to:

1. **Not using HTTPS** - The Web Speech API requires a secure context (HTTPS or localhost)
   - Solution: Deploy your app with HTTPS or test on localhost
   
2. **No internet connection** - The browser's speech recognition uses cloud services
   - Solution: Check your internet connection
   
3. **Firewall blocking** - Corporate firewalls may block the speech service
   - Solution: Try on a different network or disable firewall temporarily
   
4. **Browser limitations** - Some browsers have stricter requirements
   - Solution: Use Chrome or Edge for best results

The component automatically retries up to 3 times on network errors with helpful progress messages.

### Microphone Not Working

1. Check browser permissions
2. Ensure microphone is connected and working
3. Try in a different browser (Chrome recommended)
4. Grant microphone permission in browser settings

### No Speech Detected

1. Speak clearly and at normal volume
2. Check if microphone is muted
3. Try moving closer to the microphone
4. Test your microphone in other apps to ensure it works

### Browser Not Supported

- Use Chrome, Edge, or Safari
- Check browser version (needs to be recent)
- Enable JavaScript if disabled
- Firefox has limited support - use Chrome instead

## Examples

See the demo page at `/voice-demo` for complete working examples.

