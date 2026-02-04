
# Generate a new keystore
keytool -genkey -v -keystore android/release.jks -keyalg RSA -keysize 2048 -validity 10000 -alias release

# Convert it to base64 for GitHub Secrets
base64 android/release.jks > android/release_jks_base64.txt

echo 'Keystore generated at android/release.jks'
echo 'Base64 string saved to android/release_jks_base64.txt (copy content to RELEASE_KEYSTORE secret)'

