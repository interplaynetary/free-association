import type { LayoutServerLoad } from './$types';
import { supabase } from '$lib/supabaseClient';
export const ssr = false;

export const load: LayoutServerLoad = async () => {
	const { data } = await supabase.from('YOUR_TABLE').select();

	return {
		supabaseData: data ?? []
	};
};
