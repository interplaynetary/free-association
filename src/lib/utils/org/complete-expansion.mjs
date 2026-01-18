/**
 * Complete Expansion of ALL 143 COP30 Organizations to 6+ Priorities
 * Systematic approach for remaining organizations
 */

import fs from 'fs';

const trees = JSON.parse(fs.readFileSync('./src/lib/demo/orgs.json', 'utf8'));
const c = (id, pts) => ({ id, points: pts });

function expandOrg(slug, additionalPriorities) {
  if (!trees[slug]) return;

  const tree = trees[slug];
  const currentPriorities = tree.tree.children.length;

  if (currentPriorities >= 6) {
    return; // Already has enough
  }

  additionalPriorities.forEach(priority => {
    const newPriority = {
      id: `${slug}_root_${priority.id}`,
      name: priority.name,
      type: "NonRootNode",
      manual_fulfillment: null,
      points: priority.points,
      parent_id: `${slug}_root`,
      contributors: priority.rootContributors || [],
      anti_contributors: [],
      children: priority.subs.map(s => ({
        id: `${slug}_root_${priority.id}_${s.i}`,
        name: s.n,
        type: "NonRootNode",
        manual_fulfillment: null,
        points: s.p,
        parent_id: `${slug}_root_${priority.id}`,
        contributors: s.c || [],
        anti_contributors: [],
        children: []
      }))
    };

    tree.tree.children.push(newPriority);
  });

  console.log(`  ✓ ${slug}: ${currentPriorities} → ${tree.tree.children.length} priorities`);
}

console.log('🚀 Completing expansion of remaining organizations...\n');

// Generic priority templates for different org types
const countryPriorities = (africanBank, asianBank, iadb) => [
  {
    id: 'agriculture', name: 'Climate-Smart Agriculture', points: 15,
    subs: [
      { i: 'farming', n: 'Sustainable Farming Practices', p: 60, c: [c('org_demo_fao', 40)] },
      { i: 'livestock', n: 'Climate Livestock Management', p: 40, c: [c('org_demo_fao', 35)] }
    ]
  },
  {
    id: 'water_mgmt', name: 'Water Resource Management', points: 15,
    subs: [
      { i: 'conservation', n: 'Water Conservation', p: 100, c: [c('org_demo_siwi', 35), c('org_demo_undp', 25)] }
    ]
  },
  {
    id: 'biodiversity', name: 'Biodiversity Conservation', points: 15,
    subs: [
      { i: 'ecosystems', n: 'Ecosystem Protection', p: 100, c: [c('org_demo_wwf', 35), c('org_demo_unep', 30)] }
    ]
  }
];

// Youth organizations expansion
const youthExpansion = [
  {
    id: 'advocacy', name: 'Youth Climate Advocacy', points: 15,
    subs: [
      { i: 'campaigns', n: 'Advocacy Campaigns', p: 100, c: [c('org_demo_unicef', 35), c('org_demo_unfccc', 25)] }
    ]
  },
  {
    id: 'innovation', name: 'Youth Innovation', points: 15,
    subs: [
      { i: 'solutions', n: 'Youth-Led Solutions', p: 100, c: [c('org_demo_undp', 30)] }
    ]
  },
  {
    id: 'networks', name: 'Youth Networks', points: 10,
    subs: [
      { i: 'collaboration', n: 'International Collaboration', p: 100, c: [] }
    ]
  }
];

// Research org expansion
const researchExpansion = [
  {
    id: 'policy', name: 'Climate Policy Research', points: 15,
    subs: [
      { i: 'analysis', n: 'Policy Analysis', p: 100, c: [c('org_demo_ipcc', 35), c('org_demo_unfccc', 30)] }
    ]
  },
  {
    id: 'data', name: 'Climate Data Systems', points: 15,
    subs: [
      { i: 'collection', n: 'Data Collection & Analysis', p: 100, c: [c('org_demo_wmo', 30)] }
    ]
  },
  {
    id: 'collaboration', name: 'Research Collaboration', points: 10,
    subs: [
      { i: 'networks', n: 'Research Networks', p: 100, c: [] }
    ]
  }
];

console.log('═══ YOUTH ORGANIZATIONS ═══');

['yle', 'iync', 'ycla', 'children_youth_pavilion', 'yilaa'].forEach(org => {
  expandOrg(org, youthExpansion);
});

console.log('\n═══ RESEARCH & ACADEMIC ═══');

expandOrg('monterrey', researchExpansion);
expandOrg('deval', researchExpansion);
expandOrg('tsinghua', researchExpansion);
expandOrg('ibam', researchExpansion);

console.log('\n═══ AFRICAN COUNTRIES ═══');

const africanExpansion = countryPriorities('org_demo_africandevbank', null, null);

['ethiopia', 'namibia', 'tanzania', 'liberia', 'sierra_leone', 'cote_ivoire', 'mali',
  'malawi', 'djibouti', 'congo_drc', 'senegal', 'chad', 'rwanda', 'gabon', 'angola',
  'guinea', 'mauritania', 'uganda', 'nigeria'].forEach(country => {
    expandOrg(country, africanExpansion);
  });

console.log('\n═══ ASIAN COUNTRIES ═══');

const asianExpansion = [
  {
    id: 'disaster_resilience', name: 'Disaster Resilience', points: 15,
    subs: [
      { i: 'preparedness', n: 'Disaster Preparedness', p: 100, c: [c('org_demo_cdri', 35), c('org_demo_wmo', 30)] }
    ]
  },
  {
    id: 'urban_development', name: 'Sustainable Urban Development', points: 15,
    subs: [
      { i: 'cities', n: 'Smart Cities', p: 100, c: [c('org_demo_c40cities', 35), c('org_demo_iclei', 30)] }
    ]
  },
  {
    id: 'technology', name: 'Climate Technology', points: 15,
    subs: [
      { i: 'innovation', n: 'Tech Innovation', p: 100, c: [c('org_demo_irena', 30)] }
    ]
  }
];

['pakistan', 'bangladesh', 'thailand', 'malaysia', 'singapore', 'indonesia',
  'mongolia', 'south_korea'].forEach(country => {
    expandOrg(country, asianExpansion);
  });

console.log('\n═══ LATIN AMERICAN COUNTRIES ═══');

const latinExpansion = [
  {
    id: 'indigenous_rights', name: 'Indigenous Peoples & Climate', points: 15,
    subs: [
      { i: 'land_rights', n: 'Land Rights Protection', p: 100, c: [c('org_demo_indigenous_pavilion', 40), c('org_demo_conservationinternational', 30)] }
    ]
  },
  {
    id: 'marine', name: 'Marine & Coastal', points: 15,
    subs: [
      { i: 'protection', n: 'Coastal Protection', p: 100, c: [c('org_demo_oceandori', 35), c('org_demo_ramsar', 25)] }
    ]
  },
  {
    id: 'transport', name: 'Sustainable Transport', points: 15,
    subs: [
      { i: 'public', n: 'Public Transport Systems', p: 100, c: [c('org_demo_iclei', 35)] }
    ]
  }
];

['peru', 'colombia', 'el_salvador', 'uruguay', 'venezuela', 'chile'].forEach(country => {
  expandOrg(country, latinExpansion);
});

console.log('\n═══ EUROPEAN COUNTRIES ═══');

const europeanExpansion = [
  {
    id: 'green_deal', name: 'European Green Deal', points: 15,
    subs: [
      { i: 'implementation', n: 'Policy Implementation', p: 100, c: [c('org_demo_europeanunion', 45), c('org_demo_unfccc', 25)] }
    ]
  },
  {
    id: 'just_transition', name: 'Just Transition', points: 15,
    subs: [
      { i: 'regions', n: 'Transition Regions Support', p: 100, c: [c('org_demo_europeanunion', 40), c('org_demo_ilo', 30)] }
    ]
  },
  {
    id: 'technology', name: 'Green Technology Leadership', points: 15,
    subs: [
      { i: 'rnd', n: 'R&D Investment', p: 100, c: [c('org_demo_irena', 35)] }
    ]
  }
];

['sweden', 'finland', 'spain', 'portugal', 'italy', 'luxembourg', 'iceland',
  'ukraine', 'denmark', 'france'].forEach(country => {
    expandOrg(country, europeanExpansion);
  });

console.log('\n═══ MAJOR ECONOMIES ═══');

// China
expandOrg('china', [
  {
    id: 'coal_transition', name: 'Coal Phase-Out', points: 15,
    subs: [
      { i: 'alternatives', n: 'Clean Alternatives', p: 100, c: [c('org_demo_irena', 40), c('org_demo_worldbank', 30)] }
    ]
  },
  {
    id: 'green_belt', name: 'Green Belt Initiative', points: 15,
    subs: [
      { i: 'investments', n: 'Green Investments', p: 100, c: [c('org_demo_undp', 30)] }
    ]
  },
  {
    id: 'air_quality', name: 'Air Quality', points: 10,
    subs: [
      { i: 'pollution', n: 'Pollution Control', p: 100, c: [c('org_demo_unep', 35), c('org_demo_who', 25)] }
    ]
  }
]);

// India
expandOrg('india', [
  {
    id: 'coal_mining', name: 'Coal Sector Transition', points: 15,
    subs: [
      { i: 'workers', n: 'Coal Worker Transition', p: 100, c: [c('org_demo_ilo', 35), c('org_demo_worldbank', 25)] }
    ]
  },
  {
    id: 'agriculture', name: 'Agricultural Resilience', points: 15,
    subs: [
      { i: 'farmers', n: 'Farmer Support Programs', p: 100, c: [c('org_demo_fao', 40), c('org_demo_asiandevbank', 30)] }
    ]
  },
  {
    id: 'air_pollution', name: 'Air Pollution Control', points: 10,
    subs: [
      { i: 'mitigation', n: 'Pollution Mitigation', p: 100, c: [c('org_demo_unep', 35), c('org_demo_who', 30)] }
    ]
  }
]);

// Germany
expandOrg('germany', [
  {
    id: 'coal_exit', name: 'Coal Exit', points: 15,
    subs: [
      { i: 'timeline', n: 'Phase-Out Implementation', p: 100, c: [c('org_demo_europeanunion', 40), c('org_demo_ilo', 30)] }
    ]
  },
  {
    id: 'circular_economy', name: 'Circular Economy', points: 15,
    subs: [
      { i: 'systems', n: 'Circular Systems', p: 100, c: [c('org_demo_unep', 35), c('org_demo_wgeo', 25)] }
    ]
  },
  {
    id: 'electromobility', name: 'E-Mobility', points: 10,
    subs: [
      { i: 'infrastructure', n: 'Charging Infrastructure', p: 100, c: [c('org_demo_irena', 30)] }
    ]
  }
]);

// UK
expandOrg('uk', [
  {
    id: 'blue_economy', name: 'Blue Economy', points: 15,
    subs: [
      { i: 'marine', n: 'Marine Protection', p: 100, c: [c('org_demo_oceandori', 40), c('org_demo_ramsar', 30)] }
    ]
  },
  {
    id: 'buildings', name: 'Building Retrofit', points: 15,
    subs: [
      { i: 'efficiency', n: 'Energy Efficiency Retrofit', p: 100, c: [c('org_demo_iso', 35), c('org_demo_iclei', 25)] }
    ]
  },
  {
    id: 'peatlands', name: 'Peatland Restoration', points: 10,
    subs: [
      { i: 'restoration', n: 'Bog Restoration', p: 100, c: [c('org_demo_ramsar', 40), c('org_demo_wwf', 30)] }
    ]
  }
]);

// Brazil
expandOrg('brazil', [
  {
    id: 'cerrado', name: 'Cerrado Protection', points: 15,
    subs: [
      { i: 'conservation', n: 'Savanna Conservation', p: 100, c: [c('org_demo_wwf', 40), c('org_demo_conservationinternational', 35)] }
    ]
  },
  {
    id: 'atlantic_forest', name: 'Atlantic Forest', points: 15,
    subs: [
      { i: 'restoration', n: 'Forest Restoration', p: 100, c: [c('org_demo_natureconservancy', 35), c('org_demo_fao', 30)] }
    ]
  },
  {
    id: 'sugarcane', name: 'Sustainable Biofuels', points: 10,
    subs: [
      { i: 'ethanol', n: 'Ethanol Production', p: 100, c: [c('org_demo_unica', 40), c('org_demo_irena', 25)] }
    ]
  }
]);

// Australia
expandOrg('australia', [
  {
    id: 'reef', name: 'Great Barrier Reef', points: 15,
    subs: [
      { i: 'protection', n: 'Reef Protection', p: 100, c: [c('org_demo_oceandori', 40), c('org_demo_unep', 30)] }
    ]
  },
  {
    id: 'bushfires', name: 'Bushfire Management', points: 15,
    subs: [
      { i: 'prevention', n: 'Fire Prevention', p: 100, c: [c('org_demo_cdri', 35), c('org_demo_wmo', 30)] }
    ]
  },
  {
    id: 'mining', name: 'Sustainable Mining', points: 10,
    subs: [
      { i: 'transition', n: 'Mining Transition', p: 100, c: [c('org_demo_ilo', 30)] }
    ]
  }
]);

console.log('\n═══ MIDDLE EAST COUNTRIES ═══');

['saudi_arabia', 'qatar', 'oman', 'morocco', 'azerbaijan'].forEach(country => {
  expandOrg(country, [
    {
      id: 'desalination', name: 'Water Desalination', points: 15,
      subs: [
        { i: 'renewable', n: 'Renewable-Powered Desalination', p: 100, c: [c('org_demo_irena', 40), c('org_demo_siwi', 30)] }
      ]
    },
    {
      id: 'heat', name: 'Extreme Heat Adaptation', points: 15,
      subs: [
        { i: 'cooling', n: 'Urban Cooling', p: 100, c: [c('org_demo_c40cities', 35), c('org_demo_who', 30)] }
      ]
    },
    {
      id: 'diversification', name: 'Economic Diversification', points: 15,
      subs: [
        { i: 'non_oil', n: 'Post-Oil Economy', p: 100, c: [c('org_demo_undp', 35), c('org_demo_wgeo', 25)] }
      ]
    }
  ]);
});

console.log('\n═══ DEVELOPMENT BANKS ═══');

// AfDB
expandOrg('afdb', [
  {
    id: 'green_infrastructure', name: 'Green Infrastructure', points: 15,
    subs: [
      { i: 'projects', n: 'Infrastructure Projects', p: 100, c: [c('org_demo_cdri', 35), c('org_demo_worldbank', 30)] }
    ]
  },
  {
    id: 'private_sector', name: 'Private Sector Engagement', points: 15,
    subs: [
      { i: 'investment', n: 'Private Climate Investment', p: 100, c: [c('org_demo_icc_chamber', 35), c('org_demo_wemeanbus', 25)] }
    ]
  },
  {
    id: 'knowledge', name: 'Knowledge Products', points: 10,
    subs: [
      { i: 'research', n: 'Climate Research', p: 100, c: [c('org_demo_ipcc', 30)] }
    ]
  }
]);

// IADB
expandOrg('iadb', [
  {
    id: 'nature_solutions', name: 'Nature-Based Solutions', points: 15,
    subs: [
      { i: 'ecosystems', n: 'Ecosystem Restoration', p: 100, c: [c('org_demo_conservationinternational', 40), c('org_demo_fao', 30)] }
    ]
  },
  {
    id: 'cities', name: 'Sustainable Cities', points: 15,
    subs: [
      { i: 'urban', n: 'Urban Climate Action', p: 100, c: [c('org_demo_c40cities', 40), c('org_demo_iclei', 35)] }
    ]
  },
  {
    id: 'disaster', name: 'Disaster Risk', points: 10,
    subs: [
      { i: 'reduction', n: 'Risk Reduction', p: 100, c: [c('org_demo_cdri', 40), c('org_demo_wmo', 30)] }
    ]
  }
]);

// CAF
expandOrg('caf', [
  {
    id: 'water', name: 'Water Infrastructure', points: 15,
    subs: [
      { i: 'systems', n: 'Water Systems', p: 100, c: [c('org_demo_siwi', 40), c('org_demo_iadb', 30)] }
    ]
  },
  {
    id: 'transport', name: 'Sustainable Transport', points: 15,
    subs: [
      { i: 'systems', n: 'Transport Systems', p: 100, c: [c('org_demo_iclei', 35)] }
    ]
  },
  {
    id: 'innovation', name: 'Innovation Funding', points: 10,
    subs: [
      { i: 'startups', n: 'Climate Startups', p: 100, c: [c('org_demo_arapyau', 30)] }
    ]
  }
]);

// KFW
expandOrg('kfw', [
  {
    id: 'partnerships', name: 'Development Partnerships', points: 15,
    subs: [
      { i: 'bilateral', n: 'Bilateral Cooperation', p: 100, c: [c('org_demo_worldbank', 40), c('org_demo_undp', 30)] }
    ]
  },
  {
    id: 'green_bonds', name: 'Green Bond Issuance', points: 15,
    subs: [
      { i: 'market', n: 'Bond Market Development', p: 100, c: [c('org_demo_greenclimatefund', 35)] }
    ]
  },
  {
    id: 'evaluation', name: 'Impact Evaluation', points: 10,
    subs: [
      { i: 'metrics', n: 'Impact Metrics', p: 100, c: [c('org_demo_deval', 40)] }
    ]
  }
]);

console.log('\n═══ NGOs & CIVIL SOCIETY ═══');

// CAN
expandOrg('can', [
  {
    id: 'monitoring', name: 'Climate Policy Monitoring', points: 15,
    subs: [
      { i: 'tracking', n: 'Policy Tracking', p: 100, c: [c('org_demo_unfccc', 40), c('org_demo_climateactionnetwork', 35)] }
    ]
  },
  {
    id: 'capacity_building', name: 'NGO Capacity Building', points: 15,
    subs: [
      { i: 'support', n: 'NGO Support Programs', p: 100, c: [c('org_demo_oxfam', 30)] }
    ]
  },
  {
    id: 'communications', name: 'Climate Communications', points: 10,
    subs: [
      { i: 'messaging', n: 'Public Messaging', p: 100, c: [] }
    ]
  }
]);

// Indigenous Pavilion
expandOrg('indigenous_pavilion', [
  {
    id: 'knowledge', name: 'Traditional Knowledge', points: 15,
    subs: [
      { i: 'integration', n: 'Knowledge Integration', p: 100, c: [c('org_demo_ipcc', 35), c('org_demo_wipo', 30)] }
    ]
  },
  {
    id: 'finance', name: 'Direct Access Finance', points: 15,
    subs: [
      { i: 'mechanisms', n: 'Financial Mechanisms', p: 100, c: [c('org_demo_greenclimatefund', 40), c('org_demo_conservationinternational', 30)] }
    ]
  },
  {
    id: 'networks', name: 'Indigenous Networks', points: 10,
    subs: [
      { i: 'coordination', n: 'Network Coordination', p: 100, c: [c('org_demo_wipo', 40)] }
    ]
  }
]);

// IPAM
expandOrg('ipam', [
  {
    id: 'monitoring', name: 'Forest Monitoring', points: 15,
    subs: [
      { i: 'satellite', n: 'Satellite Monitoring', p: 100, c: [c('org_demo_ipcc', 35), c('org_demo_fao', 30)] }
    ]
  },
  {
    id: 'partnerships', name: 'Research Partnerships', points: 15,
    subs: [
      { i: 'collaboration', n: 'International Collaboration', p: 100, c: [c('org_demo_conservationinternational', 35)] }
    ]
  },
  {
    id: 'education', name: 'Environmental Education', points: 10,
    subs: [
      { i: 'programs', n: 'Education Programs', p: 100, c: [c('org_demo_brazil', 30)] }
    ]
  }
]);

// LDC Group
expandOrg('ldc_group', [
  {
    id: 'negotiations', name: 'Climate Negotiations', points: 15,
    subs: [
      { i: 'coordination', n: 'Negotiation Coordination', p: 100, c: [c('org_demo_unfccc', 50), c('org_demo_aosis', 35)] }
    ]
  },
  {
    id: 'vulnerability', name: 'Vulnerability Assessment', points: 15,
    subs: [
      { i: 'analysis', n: 'Risk Analysis', p: 100, c: [c('org_demo_ipcc', 40), c('org_demo_wmo', 30)] }
    ]
  },
  {
    id: 'south_south', name: 'South-South Cooperation', points: 10,
    subs: [
      { i: 'exchange', n: 'Knowledge Exchange', p: 100, c: [c('org_demo_undp', 35)] }
    ]
  }
]);

// Other NGOs
expandOrg('cni', [
  {
    id: 'clean_tech', name: 'Clean Technology', points: 15,
    subs: [
      { i: 'deployment', n: 'Tech Deployment', p: 100, c: [c('org_demo_irena', 35)] }
    ]
  },
  {
    id: 'efficiency', name: 'Industrial Efficiency', points: 15,
    subs: [
      { i: 'energy', n: 'Energy Efficiency', p: 100, c: [c('org_demo_iso', 30)] }
    ]
  },
  {
    id: 'competitiveness', name: 'Green Competitiveness', points: 15,
    subs: [
      { i: 'markets', n: 'Market Access', p: 100, c: [c('org_demo_wemeanbus', 25)] }
    ]
  }
]);

expandOrg('unica', [
  {
    id: 'rnd', name: 'Biofuel R&D', points: 15,
    subs: [
      { i: 'research', n: 'Research Programs', p: 100, c: [c('org_demo_irena', 30)] }
    ]
  },
  {
    id: 'land_use', name: 'Sustainable Land Use', points: 15,
    subs: [
      { i: 'practices', n: 'Best Practices', p: 100, c: [c('org_demo_fao', 35)] }
    ]
  },
  {
    id: 'export', name: 'Biofuel Export', points: 15,
    subs: [
      { i: 'markets', n: 'International Markets', p: 100, c: [] }
    ]
  }
]);

expandOrg('sitawi', [
  {
    id: 'bonds', name: 'Green Bonds', points: 15,
    subs: [
      { i: 'issuance', n: 'Bond Issuance Support', p: 100, c: [c('org_demo_greenclimatefund', 30)] }
    ]
  },
  {
    id: 'capacity', name: 'Financial Capacity', points: 15,
    subs: [
      { i: 'training', n: 'Financial Training', p: 100, c: [c('org_demo_iadb', 25)] }
    ]
  },
  {
    id: 'evaluation', name: 'Impact Evaluation', points: 15,
    subs: [
      { i: 'assessment', n: 'Impact Assessment', p: 100, c: [] }
    ]
  }
]);

expandOrg('arapyau', [
  {
    id: 'entrepreneurship', name: 'Green Entrepreneurship', points: 15,
    subs: [
      { i: 'incubation', n: 'Startup Incubation', p: 100, c: [c('org_demo_wgeo', 25)] }
    ]
  },
  {
    id: 'networks', name: 'Innovation Networks', points: 15,
    subs: [
      { i: 'collaboration', n: 'Network Building', p: 100, c: [c('org_demo_iadb', 25)] }
    ]
  },
  {
    id: 'finance', name: 'Innovation Finance', points: 15,
    subs: [
      { i: 'funding', n: 'Startup Funding', p: 100, c: [c('org_demo_sitawi', 30)] }
    ]
  }
]);

expandOrg('open_society', [
  {
    id: 'transparency', name: 'Climate Transparency', points: 15,
    subs: [
      { i: 'accountability', n: 'Government Accountability', p: 100, c: [c('org_demo_unfccc', 35)] }
    ]
  },
  {
    id: 'human_rights', name: 'Climate Human Rights', points: 15,
    subs: [
      { i: 'protection', n: 'Rights Protection', p: 100, c: [c('org_demo_oxfam', 35)] }
    ]
  },
  {
    id: 'media', name: 'Climate Media', points: 15,
    subs: [
      { i: 'journalism', n: 'Climate Journalism', p: 100, c: [] }
    ]
  }
]);

expandOrg('oceandori', [
  {
    id: 'pollution', name: 'Ocean Pollution', points: 15,
    subs: [
      { i: 'plastic', n: 'Plastic Pollution', p: 100, c: [c('org_demo_unep', 40)] }
    ]
  },
  {
    id: 'acidification', name: 'Ocean Acidification', points: 15,
    subs: [
      { i: 'monitoring', n: 'Acidification Monitoring', p: 100, c: [c('org_demo_ipcc', 35), c('org_demo_wmo', 25)] }
    ]
  },
  {
    id: 'restoration', name: 'Marine Restoration', points: 15,
    subs: [
      { i: 'reefs', n: 'Coral Reef Restoration', p: 100, c: [c('org_demo_conservationinternational', 35)] }
    ]
  }
]);

expandOrg('wipo', [
  {
    id: 'advocacy', name: 'Indigenous Advocacy', points: 15,
    subs: [
      { i: 'unfccc', n: 'UNFCCC Engagement', p: 100, c: [c('org_demo_unfccc', 40), c('org_demo_indigenous_pavilion', 35)] }
    ]
  },
  {
    id: 'capacity', name: 'Community Capacity', points: 15,
    subs: [
      { i: 'building', n: 'Capacity Building', p: 100, c: [c('org_demo_undp', 30)] }
    ]
  },
  {
    id: 'documentation', name: 'Knowledge Documentation', points: 15,
    subs: [
      { i: 'preservation', n: 'Traditional Knowledge', p: 100, c: [c('org_demo_ipcc', 25)] }
    ]
  }
]);

console.log('\n═══ REGIONAL ORGANIZATIONS ═══');

expandOrg('eac', [
  {
    id: 'transport', name: 'Regional Transport', points: 15,
    subs: [
      { i: 'corridors', n: 'Green Transport Corridors', p: 100, c: [c('org_demo_africandevbank', 35)] }
    ]
  },
  {
    id: 'energy_grid', name: 'Regional Energy Grid', points: 15,
    subs: [
      { i: 'interconnection', n: 'Grid Interconnection', p: 100, c: [c('org_demo_irena', 35), c('org_demo_africandevbank', 30)] }
    ]
  },
  {
    id: 'wildlife', name: 'Wildlife Corridors', points: 15,
    subs: [
      { i: 'transboundary', n: 'Transboundary Conservation', p: 100, c: [c('org_demo_wwf', 40), c('org_demo_unep', 30)] }
    ]
  }
]);

expandOrg('oif', [
  {
    id: 'language', name: 'French Language Climate Content', points: 15,
    subs: [
      { i: 'resources', n: 'Educational Resources', p: 100, c: [c('org_demo_france', 30)] }
    ]
  },
  {
    id: 'sahel', name: 'Sahel Climate Action', points: 15,
    subs: [
      { i: 'desertification', n: 'Anti-Desertification', p: 100, c: [c('org_demo_fao', 35), c('org_demo_africandevbank', 25)] }
    ]
  },
  {
    id: 'small_islands', name: 'Francophone Small Islands', points: 15,
    subs: [
      { i: 'support', n: 'Island Nation Support', p: 100, c: [c('org_demo_aosis', 35)] }
    ]
  }
]);

expandOrg('climate_funds', [
  {
    id: 'mdb_collaboration', name: 'MDB Collaboration', points: 15,
    subs: [
      { i: 'coordination', n: 'Fund Coordination', p: 100, c: [c('org_demo_worldbank', 45), c('org_demo_greenclimatefund', 40)] }
    ]
  },
  {
    id: 'innovation', name: 'Financial Innovation', points: 15,
    subs: [
      { i: 'instruments', n: 'Innovative Instruments', p: 100, c: [c('org_demo_greenclimatefund', 40)] }
    ]
  },
  {
    id: 'reporting', name: 'Climate Finance Reporting', points: 10,
    subs: [
      { i: 'standards', n: 'Reporting Standards', p: 100, c: [c('org_demo_unfccc', 40)] }
    ]
  }
]);

expandOrg('multilateral_banks', [
  {
    id: 'alignment', name: 'Paris Alignment', points: 15,
    subs: [
      { i: 'portfolios', n: 'Portfolio Alignment', p: 100, c: [c('org_demo_unfccc', 40)] }
    ]
  },
  {
    id: 'blended_finance', name: 'Blended Finance', points: 15,
    subs: [
      { i: 'mechanisms', n: 'Blending Mechanisms', p: 100, c: [c('org_demo_greenclimatefund', 40)] }
    ]
  },
  {
    id: 'knowledge', name: 'Knowledge Products', points: 10,
    subs: [
      { i: 'sharing', n: 'Best Practice Sharing', p: 100, c: [] }
    ]
  }
]);

expandOrg('ens', [
  {
    id: 'smr', name: 'Small Modular Reactors', points: 15,
    subs: [
      { i: 'deployment', n: 'SMR Deployment', p: 100, c: [c('org_demo_iaea', 40)] }
    ]
  },
  {
    id: 'workforce', name: 'Nuclear Workforce', points: 15,
    subs: [
      { i: 'training', n: 'Workforce Training', p: 100, c: [c('org_demo_iaea', 35)] }
    ]
  },
  {
    id: 'public_acceptance', name: 'Public Acceptance', points: 15,
    subs: [
      { i: 'engagement', n: 'Public Engagement', p: 100, c: [] }
    ]
  }
]);

expandOrg('norroway_org', [
  {
    id: 'ocean', name: 'Ocean Climate Finance', points: 15,
    subs: [
      { i: 'blue_carbon', n: 'Blue Carbon Finance', p: 100, c: [c('org_demo_oceandori', 40), c('org_demo_ramsar', 30)] }
    ]
  },
  {
    id: 'arctic', name: 'Arctic Climate', points: 15,
    subs: [
      { i: 'monitoring', n: 'Arctic Monitoring', p: 100, c: [c('org_demo_wmo', 35), c('org_demo_ipcc', 30)] }
    ]
  },
  {
    id: 'technology', name: 'Clean Technology Finance', points: 10,
    subs: [
      { i: 'innovation', n: 'Tech Innovation Support', p: 100, c: [c('org_demo_irena', 35)] }
    ]
  }
]);

// Remaining smaller orgs
['wna', 'sdce', 'climate_live', 'iaai', 'cuba', 'turkiye'].forEach(org => {
  expandOrg(org, [
    {
      id: 'partnerships', name: 'Strategic Partnerships', points: 15,
      subs: [{ i: 'collaboration', n: 'Partner Collaboration', p: 100, c: [] }]
    },
    {
      id: 'capacity', name: 'Capacity Development', points: 15,
      subs: [{ i: 'programs', n: 'Development Programs', p: 100, c: [c('org_demo_undp', 30)] }]
    },
    {
      id: 'knowledge', name: 'Knowledge Sharing', points: 15,
      subs: [{ i: 'dissemination', n: 'Information Dissemination', p: 100, c: [] }]
    }
  ]);
});

// Save final version
fs.writeFileSync('./src/lib/demo/orgs.json', JSON.stringify(trees, null, 2), 'utf8');

// Final stats
let totalOrgs = 0;
let totalPriorities = 0;
let totalSubPriorities = 0;
let totalContributors = 0;

Object.values(trees).forEach(tree => {
  totalOrgs++;
  totalPriorities += tree.tree.children.length;
  tree.tree.children.forEach(priority => {
    totalSubPriorities += priority.children.length;
    priority.children.forEach(sub => {
      totalContributors += (sub.contributors?.length || 0);
    });
  });
});

console.log('\n═══════════════════════════════════════');
console.log('✅ COMPLETE EXPANSION FINISHED!');
console.log('═══════════════════════════════════════');
console.log(`Organizations: ${totalOrgs}/143`);
console.log(`Total Priorities: ${totalPriorities} (avg ${Math.round(totalPriorities / totalOrgs)} per org)`);
console.log(`Total Sub-Priorities: ${totalSubPriorities}`);
console.log(`Total Contributor Relationships: ${totalContributors}`);
console.log('═══════════════════════════════════════\n');

