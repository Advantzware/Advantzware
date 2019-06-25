using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using Progress.Open4GL.Proxy;
using ASINET1;
using ASIDataNS;
using System.Data.SqlClient;
using System.Drawing;


public partial class folding : System.Web.UI.Page
{
    private const string ASCENDING = " ASC";
    private const string DESCENDING = " DESC";
    string raw_url;
    int estseq;

    protected void Page_PreRender(object sender, EventArgs e)
    {

        BuildDataSource();

    }
    protected void Page_Load(object sender, EventArgs e)
    {
        Session["top_spec_list_notes_index"] = null;
        Session["spec_list_notes_index"] = null;
        Session["Fold_prep_index"] = null;
        Session["Fold_prep_line"] = null;
        Session["Fold_route_index"] = null;
        Session["Fold_route_line"] = null;
        Session["fold_grid_index"] = null;
        Session["order_list_notes_index"] = null;
        Session["top_list_attach_rec_key"] = null;
        try
        {
            raw_url = Page.Request.RawUrl;
            string[] url = raw_url.Split(new char[] { '/' });
            Session["main_prgrm_url"] = url[2].ToString();
            Session["sub_prgrm_url"] = url[2].ToString();
        }
        catch { }
        if (!Page.IsPostBack)
        {
            
            CheckBox1.Checked = true;
            CheckBox2.Checked = true;
            CheckBox3.Checked = true;
            txt_l.Text = "0.00";
            txt_l2.Text = "99999";
            txt_w.Text = "0.00";
            txt_w2.Text = "99999";
            txt_d.Text = "0.00";
            txt_d2.Text = "99999";
            txt_estimate.Text = Convert.ToString(Session["prmEstimate_fold"]);
            txt_cust.Text = Convert.ToString(Session["prmCustomer_fold"]);
            txt_custpart.Text = Convert.ToString(Session["prmCustpart_fold"]);
            txt_fgitem.Text = Convert.ToString(Session["prmFgItem_fold"]);

            if (Session["prmSingle_fold"] != null)
            {
                if (Convert.ToString(Session["prmSingle_fold"]) == "Yes")
                {
                    CheckBox1.Checked = true;
                }
                if (Convert.ToString(Session["prmSingle_fold"]) == "no")
                {
                    CheckBox1.Checked = false;
                }
            }
            if (Session["prmSet_fold"] != null)
            {
                if (Convert.ToString(Session["prmSet_fold"]) == "Yes")
                {
                    CheckBox2.Checked = true;
                }
                if (Convert.ToString(Session["prmSet_fold"]) == "no")
                {
                    CheckBox2.Checked = false;
                }
            }
            if (Session["prmTandem_fold"] != null)
            {
                if (Convert.ToString(Session["prmTandem_fold"]) == "Yes")
                {
                    CheckBox3.Checked = true;
                }
                if (Convert.ToString(Session["prmTandem_fold"]) == "no")
                {
                    CheckBox3.Checked = false;
                }
            }
            BuildDataSource();
        }


        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "fold_estimate.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;
            

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            Label name = (Label)Master.FindControl("lblComp");
            Label user1 = (Label)Master.FindControl("lblUser");
            Label name2 = (Label)Master.FindControl("lbl_page");

            name.Text = PrmComp;
            user1.Text = UserLogin.UserName;
            name2.Text = "Foldings";

            if (aUsers == "external")
            {
                
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");
            }
            if (name2.Text == "Foldings")
            {
                Session["CorrugatedCartoon"] = null;
                Session["FoldingCartoon"] = 1;
                
            }

        }

        Session["Rowuser"] = UserLogin.UserName;       
        
        txt_estimate.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_cust.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_custpart.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_fgitem.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_style.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_l.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_l2.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_w.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_w2.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_d.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_d2.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_die.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_cad.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_plate.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        //txt_shipto.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        TextBox line = (TextBox)FormView1.FindControl("aLineLabel");
        line.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");

        txt_itemname.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");

        /*ImageButton brwsorder = (ImageButton)Master.FindControl("brwsCorrugated");
        brwsorder.ImageUrl = "~/images/list_estimate_1.jpg"; */    
        
        try
        {
            
            TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
            //ddl_display.Text = Convert.ToString(Session["gridsize"]);
            Session["size"] = Convert.ToInt32(ddl_display.Text);           
            GridView2.PageSize = Convert.ToInt32(Session["size"]);

            if (Session["index_list_folding"] != null)
            {
                
                GridView2.SelectedIndex = Convert.ToInt32(Session["index_list_folding"]);
                Session["order_folding_est"] = ((Label)GridView2.SelectedRow.FindControl("label_est_no")).Text;
                Session["order_entry_est_no"] = ((Label)GridView2.SelectedRow.FindControl("label_est_no")).Text;
                Session["item"] = ((Label)GridView2.SelectedRow.FindControl("Label_fgitem")).Text;
                Session["order_folding_formno"] = ((Label)GridView2.SelectedRow.FindControl("typeLabel")).Text;
                Session["order_folding_blankno"] = ((Label)GridView2.SelectedRow.FindControl("blankLabel")).Text;
                Session["order_rec_key"] = ((Label)GridView2.SelectedRow.FindControl("label_rec_key")).Text;
                Session["fold_est_print_logic_showhide"] = ((Label)GridView2.SelectedRow.FindControl("logicLabel")).Text;

            }
            
            if (Session["index_list_folding"] == null)
             {
              
                 GridView2.SelectedIndex = 0;
                 Session["order_folding_est"] = ((Label)GridView2.SelectedRow.FindControl("label_est_no")).Text;
                 Session["order_entry_est_no"] = ((Label)GridView2.SelectedRow.FindControl("label_est_no")).Text;
                 Session["item"] = ((Label)GridView2.SelectedRow.FindControl("Label_fgitem")).Text;
                 Session["order_folding_formno"] = ((Label)GridView2.SelectedRow.FindControl("typeLabel")).Text;
                 Session["order_folding_blankno"] = ((Label)GridView2.SelectedRow.FindControl("blankLabel")).Text;
                 Session["order_rec_key"] = ((Label)GridView2.SelectedRow.FindControl("label_rec_key")).Text;
                 Session["fold_est_print_logic_showhide"] = ((Label)GridView2.SelectedRow.FindControl("logicLabel")).Text;
             }
             
        }
        catch
        {
           
        }
    }
    private void BuildDataSource()
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["prmEstimate_fold"] == null || Convert.ToString(Session["prmEstimate_fold"]) == "")
            Session["prmEstimate_fold"] = "";

        if (Session["prmCustomer_fold"] == null || Convert.ToString(Session["prmCustomer_fold"]) == "")
            Session["prmCustomer_fold"] = "";
        if (Session["prmCustpart_fold"] == null || Convert.ToString(Session["prmCustpart_fold"]) == "")
            Session["prmCustpart_fold"] = "";
        if (Session["prmFgItem_fold"] == null || Convert.ToString(Session["prmFgItem_fold"]) == "")
            Session["prmFgItem_fold"] = "";
        if (Session["prmStyle_fold"] == null || Convert.ToString(Session["prmStyle_fold"]) == "")
            Session["prmStyle_fold"] = "";
        //if (Session["prmShipto_corr"] == null || Convert.ToString(Session["prmShipto_corr"]) == "")
            //Session["prmShipto_corr"] = "";
        if (Session["prmDie_fold"] == null || Convert.ToString(Session["prmDie_fold"]) == "")
            Session["prmDie_fold"] = "";
        if (Session["prmCad_fold"] == null || Convert.ToString(Session["prmCad_fold"]) == "")
            Session["prmCad_fold"] = "";
        if (Session["prmPlate_fold"] == null || Convert.ToString(Session["prmPlate_fold"]) == "")
            Session["prmPlate_fold"] = "";

        if (Session["prmSingle_fold"] == null || Convert.ToString(Session["prmSingle_fold"]) == "")
            Session["prmSingle_fold"] = "";
        if (Session["prmSet_fold"] == null || Convert.ToString(Session["prmSet_fold"]) == "")
            Session["prmSet_fold"] = "";
        if (Session["prmTandem_fold"] == null || Convert.ToString(Session["prmTandem_fold"]) == "")
            Session["prmTandem_fold"] = "";
        if (Session["prmPartDscr_fold"] == null || Convert.ToString(Session["prmPartDscr_fold"]) == "")
            Session["prmPartDscr_fold"] = "";
        if (Session["prmWidthFrom_fold"] == null || Convert.ToString(Session["prmWidthFrom_fold"]) == "")
            Session["prmWidthFrom_fold"] = 0;
        if (Session["prmWidthTo_fold"] == null || Convert.ToString(Session["prmWidthTo_fold"]) == "")
            Session["prmWidthTo_fold"] = 0;
        if (Session["prmLenFrom_fold"] == null || Convert.ToString(Session["prmLenFrom_fold"]) == "")
            Session["prmLenFrom_fold"] = 0;
        if (Session["prmLenTo_fold"] == null || Convert.ToString(Session["prmLenTo_fold"]) == "")
            Session["prmLenTo_fold"] = 0;
        if (Session["prmDepFrom_fold"] == null || Convert.ToString(Session["prmDepFrom_fold"]) == "")
            Session["prmDepFrom_fold"] = 0;
        if (Session["prmDepTo_fold"] == null || Convert.ToString(Session["prmDepTo_fold"]) == "")
            Session["prmDepTo_fold"] = 0;

        if (Convert.ToString(Session["prmAction_fold"]) != "Search" || Convert.ToString(Convert.ToString(Session["prmAction_fold"])) == "" || Session["prmAction_fold"] == null)
        {
            Session["prmAction_fold"] = "Select";
        }

        Corrugated corr = new Corrugated();
        DataSet fold_est = new DataSet();
        //Response.Write(Session["prmEstimate_corr"]);
        fold_est = corr.SelectCorrugatedBox(UserLogin.UserName, Convert.ToString(Session["prmAction_fold"]), "Folding", "", Convert.ToString(Session["prmEstimate_fold"]), Convert.ToString(Session["prmCustomer_fold"]), Convert.ToString(Session["prmCustpart_fold"]), Convert.ToString(Session["prmFgItem_fold"]), Convert.ToString(Session["prmStyle_fold"]), "", Convert.ToString(Session["prmDie_fold"]), Convert.ToString(Session["prmCad_fold"]), Convert.ToString(Session["prmPlate_fold"]), Convert.ToString(Session["prmSingle_fold"]), Convert.ToString(Session["prmSet_fold"]), Convert.ToString(Session["prmTandem_fold"]), Convert.ToDecimal(Session["prmWidthFrom_fold"]), Convert.ToDecimal(Session["prmWidthTo_fold"]), Convert.ToDecimal(Session["prmLenFrom_fold"]), Convert.ToDecimal(Session["prmLenTo_fold"]), Convert.ToDecimal(Session["prmDepFrom_fold"]), Convert.ToDecimal(Session["prmDepTo_fold"]), Convert.ToString(Session["prmPartDscr_fold"]));
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            //string cmd = "select * from column_maintain where user_name = '" + UserLogin.UserName + "' and main_program = '" + "fold_estimate.aspx" + "' and sub_program = '" + "fold_estimate.aspx" + "' order by seq ";
            string cmd = "select * from column_seq_user where user_name = '" + UserLogin.UserName + "' and main_program = '" + "fold_estimate.aspx" + "' and sub_program = '" + "fold_estimate.aspx" + "' and display = 0 order by col_seq ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count > 0)
            {
                Session["initial_val_fold_est"] = ds.Tables[0].Rows.Count;

                for (int j = 0; j < ds.Tables[0].Rows.Count; j++)
                {
                    string fld = ds.Tables[0].Rows[j][4].ToString().Trim().ToString();
                    Int32 seq = Convert.ToInt32(ds.Tables[0].Rows[j][6].ToString().Trim().ToString());
                    //Response.Write(fld + "," +seq);

                    if (fld == "vest")
                    {
                        this.estseq = Convert.ToInt32(seq);
                    }
                    if (ds.Tables[0].Rows[0][7].ToString() == "0")
                    {
                        fold_est.Tables[0].Columns[fld].SetOrdinal(seq - 1);
                    }
                }
            }
            else
            {
                string cmd_main = "select col_val, seq_no, display from column_maintenance where  main_program = '" + "fold_estimate.aspx" + "' and sub_program = '" + "fold_estimate.aspx" + "' and display = 0  order by seq_no ";
                SqlDataAdapter mainda = new SqlDataAdapter(cmd_main, conn);
                DataSet mainds = new DataSet();
                mainda.Fill(mainds);
                Session["initial_val_fold_est"] = mainds.Tables[0].Rows.Count;

                for (int j = 0; j < mainds.Tables[0].Rows.Count; j++)
                {
                    string fld = mainds.Tables[0].Rows[j][0].ToString().Trim().ToString();
                    Int32 seq = Convert.ToInt32(mainds.Tables[0].Rows[j][1].ToString().Trim().ToString());
                    //Response.Write(fld + "," + seq);

                    if (fld == "vest")
                    {
                        this.estseq = Convert.ToInt32(seq);
                    }
                    if (mainds.Tables[0].Rows[0][2].ToString() == "0")
                    {
                        fold_est.Tables[0].Columns[fld].SetOrdinal(seq - 1);
                    }
                }
            }

            for (int i = 0; i < fold_est.Tables[0].Columns.Count; i++)
            {
                if (fold_est.Tables[0].Columns[i].ColumnName == "vest")
                {
                    fold_est.Tables[0].Columns[i].ColumnName = "Estimate";
                }
                if (fold_est.Tables[0].Columns[i].ColumnName == "vCust")
                {
                    fold_est.Tables[0].Columns[i].ColumnName = "Customer";
                }
                if (fold_est.Tables[0].Columns[i].ColumnName == "vCustpart")
                {
                    fold_est.Tables[0].Columns[i].ColumnName = "Cust Part";
                }
                if (fold_est.Tables[0].Columns[i].ColumnName == "vQty")
                {
                    fold_est.Tables[0].Columns[i].ColumnName = "Qty";
                }
                if (fold_est.Tables[0].Columns[i].ColumnName == "vOrder")
                {
                    fold_est.Tables[0].Columns[i].ColumnName = "Order";
                }
                if (fold_est.Tables[0].Columns[i].ColumnName == "vFgitem")
                {
                    fold_est.Tables[0].Columns[i].ColumnName = "Fg Item";
                }
                if (fold_est.Tables[0].Columns[i].ColumnName == "vStyle")
                {
                    fold_est.Tables[0].Columns[i].ColumnName = "Style";
                }
                if (fold_est.Tables[0].Columns[i].ColumnName == "vItemName")
                {
                    fold_est.Tables[0].Columns[i].ColumnName = "Item Name";
                }
                if (fold_est.Tables[0].Columns[i].ColumnName == "vFlute")
                {
                    fold_est.Tables[0].Columns[i].ColumnName = "Paper 1";
                }
                if (fold_est.Tables[0].Columns[i].ColumnName == "vTest")
                {
                    fold_est.Tables[0].Columns[i].ColumnName = "Paper 2";
                }
                if (fold_est.Tables[0].Columns[i].ColumnName == "vQtySet")
                {
                    fold_est.Tables[0].Columns[i].ColumnName = "Qty/Set";
                }
                if (fold_est.Tables[0].Columns[i].ColumnName == "vLength")
                {
                    fold_est.Tables[0].Columns[i].ColumnName = "Length";
                }
                if (fold_est.Tables[0].Columns[i].ColumnName == "vWidth")
                {
                    fold_est.Tables[0].Columns[i].ColumnName = "Width";
                }
                if (fold_est.Tables[0].Columns[i].ColumnName == "vDepth")
                {
                    fold_est.Tables[0].Columns[i].ColumnName = "Depth";
                }
                if (fold_est.Tables[0].Columns[i].ColumnName == "vDie")
                {
                    fold_est.Tables[0].Columns[i].ColumnName = "Die#";
                }
                if (fold_est.Tables[0].Columns[i].ColumnName == "vCad")
                {
                    fold_est.Tables[0].Columns[i].ColumnName = "Cad#";
                }
                if (fold_est.Tables[0].Columns[i].ColumnName == "vPlate")
                {
                    fold_est.Tables[0].Columns[i].ColumnName = "Plate";
                }
                if (fold_est.Tables[0].Columns[i].ColumnName == "vEstDate")
                {
                    fold_est.Tables[0].Columns[i].ColumnName = "Est Date";
                }
                if (fold_est.Tables[0].Columns[i].ColumnName == "vModi")
                {
                    fold_est.Tables[0].Columns[i].ColumnName = "Modified By";
                }
                if (fold_est.Tables[0].Columns[i].ColumnName == "vShipTo")
                {
                    fold_est.Tables[0].Columns[i].ColumnName = "Ship To ID";
                }
                if (fold_est.Tables[0].Columns[i].ColumnName == "vCreatedBy")
                {
                    fold_est.Tables[0].Columns[i].ColumnName = "Created By";
                }
                if (fold_est.Tables[0].Columns[i].ColumnName == "vreckey")
                {
                    fold_est.Tables[0].Columns[i].ColumnName = "RecKey";
                }
                if (fold_est.Tables[0].Columns[i].ColumnName == "vBoard")
                {
                    fold_est.Tables[0].Columns[i].ColumnName = "Board";
                }
            }
            DataView dv = fold_est.Tables[0].DefaultView;
           
            if (Session["fold_est_gridview2_sort"] == null)
            {
                dv.Sort = "Estimate DESC";
            }
            if (Session["fold_est_gridview2_sort"] != null)
            {
                dv.Sort = Convert.ToString(Session["fold_est_gridview2_sort"]);
            }
            GridView2.DataSource = dv;
            GridView2.DataBind(); 

           
        }
        catch { }
    }

    protected void btnSearch_Click(object sender, EventArgs e)
    {
        if(CheckBox1.Checked)
        {
            HiddenField1.Value="Yes";
        }
        else
        {
            HiddenField1.Value="no";
        }
        if(CheckBox2.Checked)
        {
            HiddenField2.Value="Yes";
        }
        else
        {
            HiddenField2.Value="no";
        }
        if(CheckBox3.Checked)
        {
            HiddenField3.Value="Yes";
        }
        else
        {
            HiddenField3.Value="no";
        }
        UserClass UserLogin = (UserClass)Session["User"];
        
        Session["prmEstimate_fold"] = txt_estimate.Text.Trim();
        Session["prmAction_fold"] = "Search";
        Session["prmCustomer_fold"] = txt_cust.Text.Trim();
        Session["prmCustpart_fold"] = txt_custpart.Text.Trim();
        Session["prmFgItem_fold"] = txt_fgitem.Text.Trim();
        Session["prmStyle_fold"] = txt_style.Text.Trim();
        Session["prmDie_fold"] = txt_die.Text.Trim();
        Session["prmCad_fold"] = txt_cad.Text.Trim();
        Session["prmPlate_fold"] = txt_plate.Text.Trim();
        Session["prmWidthFrom_fold"] = txt_w.Text.Trim();
        Session["prmWidthTo_fold"] = txt_w2.Text.Trim();

        Session["prmLenFrom_fold"] = txt_l.Text.Trim();
        Session["prmLenTo_fold"] = txt_l2.Text.Trim();
        Session["prmDepFrom_fold"] = txt_d.Text.Trim();
        Session["prmDepTo_fold"] = txt_d2.Text.Trim();
        Session["prmSingle_fold"] = HiddenField1.Value;
        Session["prmSet_fold"] = HiddenField2.Value;
        Session["prmTandem_fold"] = HiddenField3.Value;
        //Session["prmShipto_fold"] = txt_shipto.Text.Trim();
        Session["prmPartDscr_fold"] = txt_itemname.Text.Trim();
        Session["index_list_folding"] = null;
        BuildDataSource();
    }



    protected void btn_reset_Click(object sender, EventArgs e)
    {
        CheckBox1.Checked = true;
        CheckBox2.Checked = true;
        CheckBox3.Checked = true;
        if (CheckBox1.Checked)
        {
            HiddenField1.Value = "Yes";
        }
        else
        {
            HiddenField1.Value = "no";
        }
        if (CheckBox2.Checked)
        {
            HiddenField2.Value = "Yes";
        }
        else
        {
            HiddenField2.Value = "no";
        }
        if (CheckBox3.Checked)
        {
            HiddenField3.Value = "Yes";
        }
        else
        {
            HiddenField3.Value = "no";
        }
        string str = "";        
        txt_estimate.Text = str.ToString();
        txt_cust.Text = str.ToString();
        txt_custpart.Text = str.ToString();
        txt_fgitem.Text = str.ToString();
        txt_style.Text = str.ToString();
        txt_die.Text = str.ToString();
        txt_cad.Text = str.ToString();
        txt_plate.Text = str.ToString();
        txt_w.Text = str.ToString();
        txt_w2.Text = str.ToString();
        txt_l.Text = str.ToString();
        txt_l2.Text = str.ToString();
        txt_d.Text = str.ToString();
        txt_d2.Text = str.ToString();
        //txt_shipto.Text = str.ToString();
        txt_itemname.Text = str.ToString();

        UserClass UserLogin = (UserClass)Session["User"];
       
        Session["prmEstimate_fold"] = txt_estimate.Text.Trim();
        Session["prmAction_fold"] = "Select";
        Session["prmCustomer_fold"] = txt_cust.Text.Trim();
        Session["prmCustpart_fold"] = txt_custpart.Text.Trim();
        Session["prmFgItem_fold"] = txt_fgitem.Text.Trim();
        Session["prmStyle_fold"] = txt_style.Text.Trim();
        Session["prmDie_fold"] = txt_die.Text.Trim();
        Session["prmCad_fold"] = txt_cad.Text.Trim();
        Session["prmPlate_fold"] = txt_plate.Text.Trim();
        Session["prmWidthFrom_fold"] = txt_w.Text.Trim();
        Session["prmWidthTo_fold"] = txt_w2.Text.Trim();
        //Session["prmShipto_fold"] = txt_shipto.Text.Trim();
        Session["prmPartDscr_fold"] = txt_itemname.Text.Trim();

        Session["prmLenFrom_fold"] = txt_l.Text.Trim();
        Session["prmLenTo_fold"] = txt_l2.Text.Trim();
        Session["prmDepFrom_fold"] = txt_d.Text.Trim();
        Session["prmDepTo_fold"] = txt_d2.Text.Trim();
        Session["prmSingle_fold"] = HiddenField1.Value;
        Session["prmSet_fold"] = HiddenField2.Value;
        Session["prmTandem_fold"] = HiddenField3.Value;
        Session["index_list_folding"] = null;
        Session["order_entry_est_no"] = null;
        Session["fold_est_gridview2_sort"] = null;

        BuildDataSource();
        //Page_Load(sender, e);
    }
       
    protected void ddl_display_TextChanged(object sender, EventArgs e)
    {
        TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
        Session["gridsize"] = ddl_display.Text;
        //ddl_display.Text = Convert.ToString(Session["gridsize"]);
        ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);

    }

    protected void GridView2_RowCreated(object sender, GridViewRowEventArgs e)
    {
        try
        {
            if (Session["initial_val_fold_est"] != null)
            {
                for (int show = Convert.ToInt32(Session["initial_val_fold_est"]) + 7; show < 36; show++)
                {
                    e.Row.Cells[show].Visible = false;
                }
            }
                      

        }
        catch { }
    }
    protected void GridView2_PageIndexChanging(object sender, GridViewPageEventArgs e)
    {
        GridView2.PageIndex = e.NewPageIndex;       
        Session["fold_est_page_index"] = e.NewPageIndex;
    }
    protected void GridView2_RowDataBound(object sender, GridViewRowEventArgs e)
    {
        if (e.Row.RowType == DataControlRowType.DataRow)
        {
            DataRowView drv = (DataRowView)e.Row.DataItem;
            for (int i = 0; i < drv.DataView.Table.Columns.Count; i++)
            {
                if (drv.DataView.Table.Columns[i].ColumnName.Equals("Est Date"))
                {
                    string[] val = e.Row.Cells[i + 7].Text.Split(new char[] { ' ' });
                    e.Row.Cells[i + 7].Text = val[0].ToString();
                }

                if (drv.DataView.Table.Columns[i].ColumnName.Equals("vModDate"))
                {
                    string val = e.Row.Cells[i + 7].Text;

                    if (val == "1")
                    {             
                        int estcellno = this.estseq + 6;

                        e.Row.Cells[estcellno].BackColor = Color.Red;
                        e.Row.Cells[estcellno].ForeColor = Color.White;                           
                    }
                }
            }
        }
        for (int i = 0; i < e.Row.Cells.Count; i++)
        {
            e.Row.Cells[i].Attributes.Add("style", "white-space: nowrap;");
        }
    }
    protected void GridView2_SelectedIndexChanged(object sender, EventArgs e)
    {
        try
        {
            Session["index_list_folding"] = GridView2.SelectedIndex;
            Session["order_folding_est"] = ((Label)GridView2.SelectedRow.FindControl("label_est_no")).Text;
            Session["order_entry_est_no"] = ((Label)GridView2.SelectedRow.FindControl("label_est_no")).Text;
            Session["item"] = ((Label)GridView2.SelectedRow.FindControl("Label_fgitem")).Text;
            Session["order_folding_formno"] = ((Label)GridView2.SelectedRow.FindControl("typeLabel")).Text;
            Session["order_folding_blankno"] = ((Label)GridView2.SelectedRow.FindControl("blankLabel")).Text;
            Session["order_rec_key"] = ((Label)GridView2.SelectedRow.FindControl("label_rec_key")).Text;
            Session["fold_est_print_logic_showhide"] = ((Label)GridView2.SelectedRow.FindControl("logicLabel")).Text;

        }
        catch { }
       
    }
    public SortDirection GridViewSortDirection
    {
        get
        {
            if (ViewState["sortDirection"] == null)
                ViewState["sortDirection"] = SortDirection.Ascending;
            return (SortDirection)ViewState["sortDirection"];
        }
        set { ViewState["sortDirection"] = value; }
    }
    protected void GridView2_Sorting(object sender, GridViewSortEventArgs e)
    {
        string sortExpression = e.SortExpression;

        if (GridViewSortDirection == SortDirection.Ascending)
        {
            GridViewSortDirection = SortDirection.Descending;
            SortGridView(sortExpression, " DESC");
        }
        else
        {
            GridViewSortDirection = SortDirection.Ascending;
            SortGridView(sortExpression, " ASC");
        }
    }
    private void SortGridView(string sortExpression, string direction)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Session["fold_est_gridview2_sort"] = sortExpression + direction;
  
    }

}
