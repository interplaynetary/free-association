/**
 * Expand ALL COP30 Organizations to 6+ Priorities
 * Add comprehensive sub-priorities and contributor networks
 */

import fs from 'fs';

const trees = JSON.parse(fs.readFileSync('./src/lib/config/org-trees.json', 'utf8'));

const c = (id, pts) => ({ id, points: pts });

// Helper to create priority structure
function createPriority(id, name, points, subs) {
  return {
    id: `{orgslug}_root_${id}`,
    name,
    type: "NonRootNode",
    manual_fulfillment: null,
    points,
    parent_id: "{orgslug}_root",
    contributors: [],
    anti_contributors: [],
    children: subs.map(s => ({
      id: `{orgslug}_root_${id}_${s.i}`,
      name: s.n,
      type: "NonRootNode",
      manual_fulfillment: null,
      points: s.p,
      parent_id: `{orgslug}_root_${id}`,
      contributors: s.c || [],
      anti_contributors: [],
      children: []
    }))
  };
}

// Helper to expand an org to 6+ priorities
function expandOrg(slug, additionalPriorities) {
  if (!trees[slug]) return;
  
  const tree = trees[slug];
  const currentPriorities = tree.tree.children.length;
  
  if (currentPriorities >= 6) {
    console.log(`  ✓ ${slug} already has ${currentPriorities} priorities`);
    return;
  }
  
  // Add new priorities
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

console.log('🚀 Expanding all organizations to 6+ priorities...\n');

// ═══════════════════════════════════════════════════════════════════
// INTERNATIONAL ORGANIZATIONS
// ═══════════════════════════════════════════════════════════════════

expandOrg('ilo', [
  { id: 'labor_standards', name: 'Labor Standards & Climate', points: 20,
    subs: [
      { i: 'safety', n: 'Workplace Safety in Climate Crisis', p: 50, c: [c('org_demo_who',30), c('org_demo_undp',25)] },
      { i: 'rights', n: 'Labor Rights Protection', p: 50, c: [c('org_demo_oxfam',30)] }
    ]
  },
  { id: 'indigenous_labor', name: 'Indigenous Workers Support', points: 15,
    subs: [
      { i: 'land_workers', n: 'Land & Forest Workers', p: 100, c: [c('org_demo_fao',35), c('org_demo_indigenous_pavilion',30)] }
    ]
  },
  { id: 'youth_employment', name: 'Youth Climate Employment', points: 15,
    subs: [
      { i: 'programs', n: 'Youth Job Programs', p: 100, c: [c('org_demo_undp',30), c('org_demo_unicef',25)] }
    ]
  }
]);

expandOrg('iaea', [
  { id: 'fusion_research', name: 'Fusion Energy Research', points: 15,
    subs: [
      { i: 'development', n: 'Fusion Development', p: 100, c: [c('org_demo_ipcc',25)] }
    ]
  },
  { id: 'cooperation', name: 'Nuclear Cooperation', points: 15,
    subs: [
      { i: 'tech_transfer', n: 'Technology Transfer', p: 100, c: [c('org_demo_wna',30), c('org_demo_ens',20)] }
    ]
  },
  { id: 'climate_modeling', name: 'Nuclear for Climate Science', points: 15,
    subs: [
      { i: 'research', n: 'Climate Research Applications', p: 100, c: [c('org_demo_ipcc',35), c('org_demo_wmo',25)] }
    ]
  }
]);

expandOrg('irena', [
  { id: 'grid_integration', name: 'Grid Integration', points: 15,
    subs: [
      { i: 'smart_grids', n: 'Smart Grid Development', p: 100, c: [c('org_demo_worldbank',30), c('org_demo_asiandevbank',25)] }
    ]
  },
  { id: 'energy_storage', name: 'Energy Storage Solutions', points: 15,
    subs: [
      { i: 'batteries', n: 'Battery Technology', p: 60, c: [c('org_demo_gatesfoundation',35)] },
      { i: 'hydrogen', n: 'Hydrogen Storage', p: 40, c: [c('org_demo_germany',30), c('org_demo_chile',25)] }
    ]
  },
  { id: 'capacity_building', name: 'Renewable Capacity Building', points: 10,
    subs: [
      { i: 'training', n: 'Technical Training Programs', p: 100, c: [c('org_demo_undp',35), c('org_demo_africandevbank',25)] }
    ]
  }
]);

expandOrg('iso', [
  { id: 'green_hydrogen', name: 'Green Hydrogen Standards', points: 15,
    subs: [
      { i: 'production', n: 'Hydrogen Production Standards', p: 100, c: [c('org_demo_irena',35), c('org_demo_germany',25)] }
    ]
  },
  { id: 'water_standards', name: 'Water Quality Standards', points: 15,
    subs: [
      { i: 'quality', n: 'Climate Water Quality', p: 100, c: [c('org_demo_siwi',35), c('org_demo_wmo',25)] }
    ]
  },
  { id: 'adaptation_standards', name: 'Adaptation Standards', points: 15,
    subs: [
      { i: 'resilience', n: 'Resilience Metrics', p: 100, c: [c('org_demo_cdri',30), c('org_demo_undp',25)] }
    ]
  }
]);

expandOrg('iom', [
  { id: 'data', name: 'Climate Migration Data', points: 20,
    subs: [
      { i: 'tracking', n: 'Displacement Tracking', p: 100, c: [c('org_demo_unhcr',35), c('org_demo_wmo',25)] }
    ]
  },
  { id: 'policy', name: 'Migration Policy', points: 15,
    subs: [
      { i: 'frameworks', n: 'Policy Frameworks', p: 100, c: [c('org_demo_unfccc',30), c('org_demo_aosis',25)] }
    ]
  },
  { id: 'livelihood', name: 'Livelihood Support', points: 15,
    subs: [
      { i: 'programs', n: 'Economic Support Programs', p: 100, c: [c('org_demo_undp',35), c('org_demo_worldbank',25)] }
    ]
  }
]);

expandOrg('wmo', [
  { id: 'ocean', name: 'Ocean Climate Monitoring', points: 20,
    subs: [
      { i: 'temps', n: 'Ocean Temperature Tracking', p: 100, c: [c('org_demo_ipcc',35), c('org_demo_oceandori',25)] }
    ]
  },
  { id: 'extreme_weather', name: 'Extreme Weather Prediction', points: 15,
    subs: [
      { i: 'forecasting', n: 'Advanced Forecasting', p: 100, c: [c('org_demo_cdri',30), c('org_demo_redcross',25)] }
    ]
  },
  { id: 'capacity', name: 'Meteorological Capacity', points: 10,
    subs: [
      { i: 'infrastructure', n: 'Weather Infrastructure', p: 100, c: [c('org_demo_worldbank',35), c('org_demo_undp',25)] }
    ]
  }
]);

expandOrg('unfccc', [
  { id: 'technology', name: 'Technology Mechanism', points: 15,
    subs: [
      { i: 'transfer', n: 'Technology Transfer', p: 100, c: [c('org_demo_unep',35), c('org_demo_irena',30)] }
    ]
  },
  { id: 'capacity_support', name: 'Capacity Building', points: 10,
    subs: [
      { i: 'developing', n: 'Developing Country Support', p: 100, c: [c('org_demo_undp',40), c('org_demo_ldc_group',30)] }
    ]
  },
  { id: 'adaptation', name: 'Global Adaptation', points: 10,
    subs: [
      { i: 'goal', n: 'Global Adaptation Goal', p: 100, c: [c('org_demo_aosis',35), c('org_demo_ldc_group',30)] }
    ]
  }
]);

expandOrg('nep', [
  { id: 'biochar', name: 'Biochar & Soil Carbon', points: 15,
    subs: [
      { i: 'production', n: 'Biochar Production', p: 100, c: [c('org_demo_fao',35), c('org_demo_ipcc',25)] }
    ]
  },
  { id: 'mineralization', name: 'Enhanced Weathering', points: 15,
    subs: [
      { i: 'deployment', n: 'Mineralization Projects', p: 100, c: [c('org_demo_ipcc',30)] }
    ]
  },
  { id: 'monitoring', name: 'Carbon Removal Monitoring', points: 15,
    subs: [
      { i: 'mrv', n: 'MRV Systems', p: 100, c: [c('org_demo_ipcc',40), c('org_demo_climate_registry',30)] }
    ]
  }
]);

expandOrg('iclei', [
  { id: 'transport', name: 'Urban Transport', points: 15,
    subs: [
      { i: 'transit', n: 'Public Transit', p: 60, c: [c('org_demo_c40cities',35), c('org_demo_worldbank',25)] },
      { i: 'cycling', n: 'Cycling Infrastructure', p: 40, c: [c('org_demo_c40cities',30)] }
    ]
  },
  { id: 'energy', name: 'City Energy Systems', points: 15,
    subs: [
      { i: 'efficiency', n: 'Building Efficiency', p: 100, c: [c('org_demo_iso',30), c('org_demo_irena',25)] }
    ]
  },
  { id: 'biodiversity', name: 'Urban Biodiversity', points: 10,
    subs: [
      { i: 'corridors', n: 'Wildlife Corridors', p: 100, c: [c('org_demo_wwf',35), c('org_demo_unep',25)] }
    ]
  }
]);

expandOrg('cdri', [
  { id: 'water_infra', name: 'Water Infrastructure', points: 15,
    subs: [
      { i: 'systems', n: 'Resilient Water Systems', p: 100, c: [c('org_demo_siwi',40), c('org_demo_worldbank',30)] }
    ]
  },
  { id: 'communications', name: 'Communication Infrastructure', points: 15,
    subs: [
      { i: 'networks', n: 'Resilient Networks', p: 100, c: [c('org_demo_wmo',30)] }
    ]
  },
  { id: 'financing', name: 'Resilience Financing', points: 15,
    subs: [
      { i: 'mechanisms', n: 'Innovative Finance', p: 100, c: [c('org_demo_worldbank',40), c('org_demo_greenclimatefund',35)] }
    ]
  }
]);

expandOrg('icc_chamber', [
  { id: 'standards', name: 'Business Standards', points: 15,
    subs: [
      { i: 'reporting', n: 'Climate Reporting', p: 100, c: [c('org_demo_iso',35), c('org_demo_climate_registry',30)] }
    ]
  },
  { id: 'innovation', name: 'Green Innovation', points: 15,
    subs: [
      { i: 'tech', n: 'Business Climate Tech', p: 100, c: [c('org_demo_irena',30), c('org_demo_gatesfoundation',25)] }
    ]
  },
  { id: 'partnerships', name: 'Public-Private Partnerships', points: 15,
    subs: [
      { i: 'collaboration', n: 'Government Collaboration', p: 100, c: [c('org_demo_worldbank',35), c('org_demo_undp',25)] }
    ]
  }
]);

expandOrg('siwi', [
  { id: 'agriculture', name: 'Water for Agriculture', points: 15,
    subs: [
      { i: 'irrigation', n: 'Efficient Irrigation', p: 100, c: [c('org_demo_fao',40), c('org_demo_worldbank',25)] }
    ]
  },
  { id: 'urban_water', name: 'Urban Water Systems', points: 15,
    subs: [
      { i: 'management', n: 'City Water Management', p: 100, c: [c('org_demo_c40cities',35), c('org_demo_iclei',25)] }
    ]
  },
  { id: 'quality', name: 'Water Quality Protection', points: 15,
    subs: [
      { i: 'pollution', n: 'Pollution Control', p: 100, c: [c('org_demo_unep',35), c('org_demo_who',25)] }
    ]
  }
]);

expandOrg('isa', [
  { id: 'manufacturing', name: 'Solar Manufacturing', points: 15,
    subs: [
      { i: 'local', n: 'Local Manufacturing', p: 100, c: [c('org_demo_india',35), c('org_demo_china',30)] }
    ]
  },
  { id: 'off_grid', name: 'Off-Grid Solar', points: 15,
    subs: [
      { i: 'rural', n: 'Rural Electrification', p: 100, c: [c('org_demo_undp',35), c('org_demo_africandevbank',30)] }
    ]
  },
  { id: 'standards', name: 'Solar Standards', points: 10,
    subs: [
      { i: 'quality', n: 'Quality Standards', p: 100, c: [c('org_demo_iso',35), c('org_demo_irena',30)] }
    ]
  }
]);

expandOrg('intosai', [
  { id: 'sdgs', name: 'SDG Climate Auditing', points: 20,
    subs: [
      { i: 'alignment', n: 'Climate-SDG Alignment', p: 100, c: [c('org_demo_undp',35), c('org_demo_unfccc',25)] }
    ]
  },
  { id: 'methodology', name: 'Audit Methodology', points: 20,
    subs: [
      { i: 'standards', n: 'Climate Audit Standards', p: 100, c: [c('org_demo_iso',30)] }
    ]
  },
  { id: 'knowledge', name: 'Knowledge Sharing', points: 15,
    subs: [
      { i: 'network', n: 'Auditor Network', p: 100, c: [] }
    ]
  }
]);

expandOrg('ramsar', [
  { id: 'restoration', name: 'Wetland Restoration', points: 20,
    subs: [
      { i: 'degraded', n: 'Degraded Wetlands', p: 100, c: [c('org_demo_unep',35), c('org_demo_fao',30)] }
    ]
  },
  { id: 'blue_carbon', name: 'Blue Carbon', points: 15,
    subs: [
      { i: 'finance', n: 'Blue Carbon Finance', p: 100, c: [c('org_demo_oceandori',35), c('org_demo_greenclimatefund',30)] }
    ]
  },
  { id: 'policy', name: 'Wetland Policy', points: 15,
    subs: [
      { i: 'integration', n: 'Policy Integration', p: 100, c: [c('org_demo_unep',35), c('org_demo_undp',25)] }
    ]
  }
]);

expandOrg('ndc_partnership', [
  { id: 'tracking', name: 'NDC Tracking', points: 15,
    subs: [
      { i: 'progress', n: 'Progress Monitoring', p: 100, c: [c('org_demo_unfccc',40), c('org_demo_ipcc',30)] }
    ]
  },
  { id: 'sector_support', name: 'Sectoral Support', points: 15,
    subs: [
      { i: 'energy', n: 'Energy Sector NDCs', p: 50, c: [c('org_demo_irena',40)] },
      { i: 'transport', n: 'Transport Sector NDCs', p: 50, c: [c('org_demo_iclei',30)] }
    ]
  },
  { id: 'innovation', name: 'NDC Innovation', points: 10,
    subs: [
      { i: 'approaches', n: 'Innovative Approaches', p: 100, c: [c('org_demo_gatesfoundation',30)] }
    ]
  }
]);

expandOrg('cfrn', [
  { id: 'market_mechanisms', name: 'Carbon Market Mechanisms', points: 15,
    subs: [
      { i: 'trading', n: 'Carbon Trading', p: 100, c: [c('org_demo_worldbank',35), c('org_demo_unfccc',30)] }
    ]
  },
  { id: 'biodiversity', name: 'Forest Biodiversity', points: 15,
    subs: [
      { i: 'conservation', n: 'Species Conservation', p: 100, c: [c('org_demo_wwf',40), c('org_demo_conservationinternational',35)] }
    ]
  },
  { id: 'capacity', name: 'Country Capacity', points: 10,
    subs: [
      { i: 'technical', n: 'Technical Capacity', p: 100, c: [c('org_demo_fao',35), c('org_demo_undp',30)] }
    ]
  }
]);

expandOrg('wwea', [
  { id: 'floating', name: 'Floating Wind', points: 15,
    subs: [
      { i: 'technology', n: 'Floating Technology', p: 100, c: [c('org_demo_irena',40), c('org_demo_uk',30)] }
    ]
  },
  { id: 'repowering', name: 'Wind Farm Repowering', points: 15,
    subs: [
      { i: 'upgrades', n: 'Turbine Upgrades', p: 100, c: [c('org_demo_germany',35), c('org_demo_denmark',30)] }
    ]
  },
  { id: 'supply_chain', name: 'Wind Supply Chain', points: 10,
    subs: [
      { i: 'local', n: 'Local Supply Chains', p: 100, c: [c('org_demo_china',30), c('org_demo_india',25)] }
    ]
  }
]);

expandOrg('climate_registry', [
  { id: 'scope3', name: 'Scope 3 Emissions', points: 15,
    subs: [
      { i: 'supply_chain', n: 'Supply Chain Emissions', p: 100, c: [c('org_demo_wemeanbus',35), c('org_demo_iso',30)] }
    ]
  },
  { id: 'forestry', name: 'Forestry Carbon', points: 15,
    subs: [
      { i: 'accounting', n: 'Forest Carbon Accounting', p: 100, c: [c('org_demo_fao',40), c('org_demo_ipcc',30)] }
    ]
  },
  { id: 'technology', name: 'Reporting Technology', points: 15,
    subs: [
      { i: 'digital', n: 'Digital Reporting Tools', p: 100, c: [c('org_demo_unfccc',30)] }
    ]
  }
]);

expandOrg('wgeo', [
  { id: 'resource_efficiency', name: 'Resource Efficiency', points: 15,
    subs: [
      { i: 'materials', n: 'Material Efficiency', p: 100, c: [c('org_demo_unep',35), c('org_demo_iso',25)] }
    ]
  },
  { id: 'blue_economy', name: 'Blue Economy', points: 15,
    subs: [
      { i: 'ocean', n: 'Ocean-Based Economy', p: 100, c: [c('org_demo_oceandori',35), c('org_demo_fao',25)] }
    ]
  },
  { id: 'metrics', name: 'Green Economy Metrics', points: 15,
    subs: [
      { i: 'indicators', n: 'Progress Indicators', p: 100, c: [c('org_demo_undp',35), c('org_demo_worldbank',25)] }
    ]
  }
]);

console.log('Batch 1 complete: International Organizations\n');

// Save periodically
fs.writeFileSync('./src/lib/config/org-trees.json', JSON.stringify(trees, null, 2), 'utf8');

console.log('✅ Enhanced international organizations - saved checkpoint');
console.log('📊 Continuing with countries and other organizations...\n');


// Continue with major countries and regions...
// This needs to be a VERY comprehensive expansion covering all 143 orgs

// First run what we have so far
console.log('Running initial batch...');
