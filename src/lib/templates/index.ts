import type { RootNode } from '$lib/commons/v5/schemas';
import { populateSDGTree } from './sdg';
import { populateNVCTree } from './nvc';

export type TemplateEntry = {
  id: string;
  label: string;
  emoji: string;
  populate: (root: RootNode) => RootNode;
};

const TEMPLATE_REGISTRY: TemplateEntry[] = [
  { id: 'sdg', label: 'Sustainable Development Goals', emoji: '🌍', populate: populateSDGTree },
  { id: 'nvc', label: 'Universal Basic Needs (NVC)', emoji: '🫶', populate: populateNVCTree }
];

export function getTemplates(): TemplateEntry[] {
  return TEMPLATE_REGISTRY.slice();
}

export function applyTemplate(root: RootNode, templateId: string): RootNode | null {
  const entry = TEMPLATE_REGISTRY.find((t) => t.id === templateId);
  if (!entry) return null;
  return entry.populate(root);
}


