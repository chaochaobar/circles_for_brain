link_ori = unique(edge_for_cirles);
link_end = unique(edge_for_cirles);
edge_all = unique([link_ori;link_end]);
edge_all_s = sort(edge_all);
edge_s = sortrows(edge_for_cirles);
name_all = name_246(edge_all_s,2);


%制作ROIID:先使用python读lobe和gyrus的名字，然后执行下面的语句
[lobe,gyrus,x,y,z,t,name_sub] = textread('for_circle.node','%s %s %d %d %d %f %s');

%%% 制作LobeID, GyrusID, ROIID, LinkID,使用excel
for i = 1:len_sig
    index_start = find(edge_all==edge_s(i, 1));
    index_end = find(edge_all==edge_s(i, 2));
    edges_all_name(i,1:2) = [name_all(index_start), name_all(index_end)];
end


%%% start lobe , end lobe 
for i = 1:len_sig
    cur_start = edges_all_name{i, 1};
    index_start = find(strcmp(C,cur_start));
    cur_end = edges_all_name{i,2};
    index_end = find(strcmp(C, cur_end));
    edges_all_name_2(i, 1) = A(index_start, 1);
    edges_all_name_2(i, 2) = A(index_end, 1);
end



%%% start lobe , end lobe 
for i = 1:len_sig
    cur_start = edges_all_name{i, 1};
    index_start = find(strcmp(C,cur_start));
    cur_end = edges_all_name{i,2};
    index_end = find(strcmp(C, cur_end));
    edges_all_name_2(i, 1) = B(index_start, 1);
    edges_all_name_2(i, 2) = B(index_end, 1);
end


%%% 制作LinkID的代码
point1 = zeros(len_sig, 1);
point2 = zeros(len_sig, 1);
for i = 1:len_sig
    cur_start = edges_all_name{i, 1};
    index_start = find(strcmp(C,cur_start));
    cur_end = edges_all_name{i,2};
    index_end = find(strcmp(C, cur_end));
    point1(i, 1) = D(index_start, 1) + 0.5;
    point2(i,1) = D(index_end, 1) + 0.5;
end





    