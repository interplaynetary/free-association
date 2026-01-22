/**
 * Static Skill Definitions for Info-Game Skill Tree
 */

export interface SkillNode {
    id: string;
    name: string;
    category: string;
    icon: string;
    description: string;
    children?: SkillNode[];
}

export const SKILL_TREE: SkillNode[] = [
    {
        id: 'transversal',
        name: 'Transversal Skills',
        category: 'transversal',
        icon: '🔄',
        description: 'Transferable skills across domains',
        children: [
            { id: 'T1', name: 'Core skills', category: 'transversal', icon: '💎', description: 'Fundamental competences' },
            { id: 'T2', name: 'Thinking skills', category: 'transversal', icon: '🧠', description: 'Critical and creative thinking' },
            { id: 'T3', name: 'Self-management', category: 'transversal', icon: '🧘', description: 'Autonomy and regulation' },
            { id: 'T4', name: 'Social & Communication', category: 'transversal', icon: '🗣️', description: 'Interacting with others' },
            { id: 'T5', name: 'Physical & Manual', category: 'transversal', icon: '🤲', description: 'Dexterity and strength' },
            { id: 'T6', name: 'Life skills', category: 'transversal', icon: '🌱', description: 'Everyday living' }
        ]
    },
    {
        id: 'skills',
        name: 'Professional Skills',
        category: 'skills',
        icon: '🛠️',
        description: 'Job-specific technical skills',
        children: [
            { id: 'S1', name: 'Communication & Creativity', category: 'skills', icon: '🎨', description: 'Artistic and verbal' },
            { id: 'S2', name: 'Information skills', category: 'skills', icon: 'ℹ️', description: 'Data and research' },
            { id: 'S3', name: 'Assisting & Caring', category: 'skills', icon: '❤️', description: 'Helping others' },
            { id: 'S4', name: 'Management', category: 'skills', icon: '📊', description: 'Leading and organizing' },
            { id: 'S5', name: 'Computers', category: 'skills', icon: '💻', description: 'Digital tools' },
            { id: 'S6', name: 'Handling & Moving', category: 'skills', icon: '📦', description: 'Logistics and manual' },
            { id: 'S7', name: 'Constructing', category: 'skills', icon: '🏗️', description: 'Building and repair' },
            { id: 'S8', name: 'Machinery', category: 'skills', icon: '🚜', description: 'Operating equipment' }
        ]
    },
    {
        id: 'knowledge',
        name: 'Knowledge',
        category: 'knowledge',
        icon: '📚',
        description: 'Academic and theoretical knowledge',
        children: [
            { id: 'K00', name: 'Generic Programs', category: 'knowledge', icon: '🎓', description: 'General education' },
            { id: 'K01', name: 'Education', category: 'knowledge', icon: '🏫', description: 'Pedagogy' },
            { id: 'K02', name: 'Arts & Humanities', category: 'knowledge', icon: '🎭', description: 'Culture and history' },
            { id: 'K03', name: 'Social Sciences', category: 'knowledge', icon: '⚖️', description: 'Society and behavior' },
            { id: 'K04', name: 'Business & Law', category: 'knowledge', icon: '💼', description: 'Commerce and regulations' },
            { id: 'K05', name: 'Natural Sciences', category: 'knowledge', icon: '🔬', description: 'Math and biology' },
            { id: 'K06', name: 'Information & Communication Technologies', category: 'knowledge', icon: '💾', description: 'Tech theory' },
            { id: 'K07', name: 'Engineering', category: 'knowledge', icon: '⚙️', description: 'Applied science' },
            { id: 'K08', name: 'Agriculture', category: 'knowledge', icon: '🌾', description: 'Farming and nature' },
            { id: 'K09', name: 'Health', category: 'knowledge', icon: '🏥', description: 'Medical knowledge' },
            { id: 'K10', name: 'Services', category: 'knowledge', icon: '🛎️', description: 'Hospitality and service' }
        ]
    },
    {
        id: 'languages',
        name: 'Languages',
        category: 'language',
        icon: '🌍',
        description: 'Linguistic capabilties',
        children: [
            { id: 'L1', name: 'Modern Languages', category: 'language', icon: '💬', description: 'Living languages' },
            { id: 'L2', name: 'Classical Languages', category: 'language', icon: '🏛️', description: 'Latin, Greek, etc.' }
        ]
    }
];
