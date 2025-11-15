const hospitalityResources = new D3Node('Hospitality Resources', null, []);

// 1. Space-Time Resources
const spaceTime = hospitalityResources.addChild("Space-Time Resources", 100);
spaceTime.addChild("Rest Spaces", 25, [
    "Couch surfing spots",
    "Floor space with mattresses",
    "Quiet reading corners",
    "Balcony relaxation areas",
    "Window nooks for contemplation"
]);
spaceTime.addChild("Work Spaces", 25, [
    "Kitchen table for laptop work",
    "Balcony as outdoor office",
    "Living room co-working",
    "Quiet study corners",
    "Standing desk arrangements"
]);
spaceTime.addChild("Social Spaces", 25, [
    "Living room gatherings",
    "Kitchen conversations",
    "Balcony discussions",
    "Hallway chance encounters",
    "Window-seat dialogues"
]);
spaceTime.addChild("Time Windows", 25, [
    "Morning quiet hours",
    "Afternoon workspace sharing",
    "Evening social windows",
    "Late-night quiet zones",
    "Flexible schedule gaps"
]);

// 2. Basic Necessities
const basicNeeds = hospitalityResources.addChild("Basic Necessities", 100);
basicNeeds.addChild("Rest Essentials", 25, [
    "Clean sheets and blankets",
    "Extra pillows",
    "Sleep masks",
    "Earplugs",
    "Fresh towels",
    "Emergency toiletries"
]);
basicNeeds.addChild("Kitchen Access", 25, [
    "Cooking space sharing",
    "Basic spices and oils",
    "Tea and coffee station",
    "Filtered water",
    "Shared condiments",
    "Communal snacks"
]);
basicNeeds.addChild("Bathroom Access", 25, [
    "Shower scheduling",
    "Basic toiletries",
    "Clean towels",
    "Hair dryer",
    "Washing machine access"
]);
basicNeeds.addChild("Storage", 25, [
    "Temporary shelf space",
    "Coat hooks",
    "Shoe storage",
    "Backpack storage",
    "Secure valuables space"
]);

// 3. Digital Infrastructure
const digitalInfra = hospitalityResources.addChild("Digital Infrastructure", 100);
digitalInfra.addChild("Connectivity", 33, [
    "Fast WiFi access",
    "Backup mobile hotspot",
    "Power strips",
    "Universal adapters",
    "Charging stations"
]);
digitalInfra.addChild("Work Tools", 33, [
    "Printer access",
    "Scanner apps",
    "External monitors",
    "Keyboard/mouse lending",
    "Webcam for calls"
]);
digitalInfra.addChild("Entertainment", 34, [
    "Netflix/streaming sharing",
    "Music systems",
    "Gaming consoles",
    "Digital library access",
    "Podcast setup"
]);

// 4. Knowledge Resources
const knowledge = hospitalityResources.addChild("Knowledge Resources", 100);
knowledge.addChild("Local Intelligence", 33, [
    "Neighborhood guides",
    "Best local spots",
    "Transport tricks",
    "Cultural calendar",
    "Free activity tips"
]);

knowledge.addChild("Network Access", 33, [
    "Local community contacts",
    "Cultural scene connections",
    "Activist networks",
    "Artist communities",
    "Professional networks"
]);
knowledge.addChild("Skill Sharing", 34, [
    "Language exchange",
    "Technical skills",
    "Creative workshops",
    "Local knowledge",
    "Professional expertise"
]);

// 5. Social Resources
const social = hospitalityResources.addChild("Social Resources", 100);
social.addChild("Community Access", 33, [
    "House dinners",
    "Movie nights",
    "Discussion circles",
    "Project collaborations",
    "Skill shares"
]);
social.addChild("Cultural Exchange", 33, [
    "Language tandems",
    "Cooking sessions",
    "Music sharing",
    "Art creation",
    "Story exchanges"
]);
social.addChild("Network Building", 34, [
    "Introduction to locals",
    "Community events",
    "Professional contacts",
    "Creative collaborations",
    "Mutual aid networks"
]);

// 6. Practical Support
const practical = hospitalityResources.addChild("Practical Support", 100);
practical.addChild("Navigation", 33, [
    "Transport cards lending",
    "Bike sharing",
    "Local guidance",
    "Emergency contacts",
    "Medical access info"
]);
practical.addChild("Administrative", 33, [
    "Address for packages",
    "Mail handling",
    "Printing/scanning",
    "Local phone number",
    "Translation help"
]);
practical.addChild("Emergency", 34, [
    "First aid supplies",
    "Medical contacts",
    "Legal resources",
    "Mental health support",
    "Crisis assistance"
]);