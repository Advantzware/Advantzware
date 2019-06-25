
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

public partial class corrugated_brow : System.Web.UI.Page
{
    private const string ASCENDING = " ASC";
    private const string DESCENDING = " DESC";
    string raw_url;
    protected void Page_PreRender(object sender, EventArgs e)
    {
        BuildDataSource();
    }

    protected void Page_Load(object sender, EventArgs e)
    {
        Session["top_spec_list_notes_index"] = null;
        Session["spec_list_notes_index"] = null;
        Session["order_list_notes_index"] = null;
        //Response.Write(Session["page_name"]);
        Session["corr_prep_index"] = null;
        Session["corr_prep_line"] = null;
        Session["corr_route_index"] = null;
        Session["corr_route_line"] = null;
        Session["index_list1_corrugated"] = null;
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
            txt_estimate.Text = Convert.ToString(Session["prmEstimate_corr"]);
            txt_cust.Text = Convert.ToString(Session["prmCustomer_corr"]);
            txt_custpart.Text = Convert.ToString(Session["prmCustpart_corr"]);
            txt_fgitem.Text = Convert.ToString(Session["prmFgItem_corr"]);

            if (Session["prmSingle_corr"] != null)
            {
                if (Convert.ToString(Session["prmSingle_corr"]) == "Yes")
                {
                    CheckBox1.Checked = true;
                }
                if (Convert.ToString(Session["prmSingle_corr"]) == "no")
                {
                    CheckBox1.Checked = false;
                }
            }
            if (Session["prmSet_corr"] != null)
            {
                if (Convert.ToString(Session["prmSet_corr"]) == "Yes")
                {
                    CheckBox2.Checked = true;
                }
                if (Convert.ToString(Session["prmSet_corr"]) == "no")
                {
                    CheckBox2.Checked = false;
                }
            }
            if (Session["prmTandem_corr"] != null)
            {
                if (Convert.ToString(Session["prmTandem_corr"]) == "Yes")
                {
                    CheckBox3.Checked = true;
                }
                if (Convert.ToString(Session["prmTandem_corr"]) == "no")
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
            string vPage = "corrugated_brow.aspx";
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
            name2.Text = "Corrugated Box";
            if (aUsers == "external")
            {
                
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");
            }
            if (name2.Text == "Corrugated Box")
            {
                Session["CorrugatedCartoon"] = 1;
                Session["FoldingCartoon"] = null;
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
        txt_shipto.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_itemname.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");


        TextBox line = (TextBox)FormView1.FindControl("aLineLabel");
        line.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");

       
        /*ImageButton brwsorder = (ImageButton)Master.FindControl("brwsCorrugated");
        brwsorder.ImageUrl = "~/images/list_estimate_1.jpg";*/     
        
        try
        {
            
            TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
            //ddl_display.Text = Convert.ToString(Session["gridsize"]);
            Session["size"] = Convert.ToInt32(ddl_display.Text);            
            GridView2.PageSize = Convert.ToInt32(Session["size"]);
            if (Session["index_list_corrugated"] != null)
            {
                
                GridView2.SelectedIndex = Convert.ToInt32(Session["index_list_corrugated"]);

                Session["order_corrugated_est"] = ((Label)GridView2.SelectedRow.FindControl("label_est_no")).Text;
                Session["order_corrugated_fgitem"] = ((Label)GridView2.SelectedRow.FindControl("Label_fgitem")).Text;
                Session["order_entry_est_no"] = ((Label)GridView2.SelectedRow.FindControl("label_est_no")).Text;
                Session["item"] = ((Label)GridView2.SelectedRow.FindControl("Label_fgitem")).Text;

                Session["order_corrugated_formno"] = ((Label)GridView2.SelectedRow.FindControl("typeLabel")).Text;
                Session["order_corrugated_blankno"] = ((Label)GridView2.SelectedRow.FindControl("blankLabel")).Text;
                Session["order_rec_key"] = ((Label)GridView2.SelectedRow.FindControl("label_rec_key")).Text;

            }
            if (Session["index_list_corrugated"] == null)
             {
               
                 GridView2.SelectedIndex = 0;

                 Session["order_corrugated_est"] = ((Label)GridView2.SelectedRow.FindControl("label_est_no")).Text;
                 Session["order_corrugated_fgitem"] = ((Label)GridView2.SelectedRow.FindControl("Label_fgitem")).Text;
                 Session["order_entry_est_no"] = ((Label)GridView2.SelectedRow.FindControl("label_est_no")).Text;
                 Session["item"] = ((Label)GridView2.SelectedRow.FindControl("Label_fgitem")).Text;

                 Session["order_corrugated_formno"] = ((Label)GridView2.SelectedRow.FindControl("typeLabel")).Text;
                 Session["order_corrugated_blankno"] = ((Label)GridView2.SelectedRow.FindControl("blankLabel")).Text;
                 Session["order_rec_key"] = ((Label)GridView2.SelectedRow.FindControl("label_rec_key")).Text;
             }
             
        }
        catch
        {
            //return;
        }
    }

    private void BuildDataSource()
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["prmEstimate_corr"] == null || Convert.ToString(Session["prmEstimate_corr"]) == "")
            Session["prmEstimate_corr"] = "";

        if (Session["prmCustomer_corr"] == null || Convert.ToString(Session["prmCustomer_corr"]) == "")
            Session["prmCustomer_corr"] = "";
        if (Session["prmCustpart_corr"] == null || Convert.ToString(Session["prmCustpart_corr"]) == "")
            Session["prmCustpart_corr"] = "";
        if (Session["prmFgItem_corr"] == null || Convert.ToString(Session["prmFgItem_corr"]) == "")
            Session["prmFgItem_corr"] = "";
        if (Session["prmStyle_corr"] == null || Convert.ToString(Session["prmStyle_corr"]) == "")
            Session["prmStyle_corr"] = "";
        if (Session["prmShipto_corr"] == null || Convert.ToString(Session["prmShipto_corr"]) == "")
            Session["prmShipto_corr"] = "";
        if (Session["prmDie_corr"] == null || Convert.ToString(Session["prmDie_corr"]) == "")
            Session["prmDie_corr"] = "";
        if (Session["prmCad_corr"] == null || Convert.ToString(Session["prmCad_corr"]) == "")
            Session["prmCad_corr"] = "";
        if (Session["prmPlate_corr"] == null || Convert.ToString(Session["prmPlate_corr"]) == "")
            Session["prmPlate_corr"] = "";

        if (Session["prmSingle_corr"] == null || Convert.ToString(Session["prmSingle_corr"]) == "")
            Session["prmSingle_corr"] = "";
        if (Session["prmSet_corr"] == null || Convert.ToString(Session["prmSet_corr"]) == "")
            Session["prmSet_corr"] = "";
        if (Session["prmTandem_corr"] == null || Convert.ToString(Session["prmTandem_corr"]) == "")
            Session["prmTandem_corr"] = "";
        if (Session["prmPartDscr_corr"] == null || Convert.ToString(Session["prmPartDscr_corr"]) == "")
            Session["prmPartDscr_corr"] = "";
        if (Session["prmWidthFrom_corr"] == null || Convert.ToString(Session["prmWidthFrom_corr"]) == "")
            Session["prmWidthFrom_corr"] = 0;
        if (Session["prmWidthTo_corr"] == null || Convert.ToString(Session["prmWidthTo_corr"]) == "")
            Session["prmWidthTo_corr"] = 0;
        if (Session["prmLenFrom_corr"] == null || Convert.ToString(Session["prmLenFrom_corr"]) == "")
            Session["prmLenFrom_corr"] = 0;
        if (Session["prmLenTo_corr"] == null || Convert.ToString(Session["prmLenTo_corr"]) == "")
            Session["prmLenTo_corr"] = 0;
        if (Session["prmDepFrom_corr"] == null || Convert.ToString(Session["prmDepFrom_corr"]) == "")
            Session["prmDepFrom_corr"] = 0;
        if (Session["prmDepTo_corr"] == null || Convert.ToString(Session["prmDepTo_corr"]) == "")
            Session["prmDepTo_corr"] = 0;

        if (Convert.ToString(Session["prmAction_corr"]) != "Search" || Convert.ToString(Convert.ToString(Session["prmAction_corr"])) == "" || Session["prmAction_corr"] == null)
        {
            Session["prmAction_corr"] = "Select";
        }
      
        Corrugated corr = new Corrugated();
        DataSet corr_est = new DataSet();
        //Response.Write(Session["prmEstimate_corr"]);
        corr_est = corr.SelectCorrugatedBox(UserLogin.UserName, Convert.ToString(Session["prmAction_corr"]), "Corr", "", Convert.ToString(Session["prmEstimate_corr"]), Convert.ToString(Session["prmCustomer_corr"]), Convert.ToString(Session["prmCustpart_corr"]),Convert.ToString(Session["prmFgItem_corr"]), Convert.ToString(Session["prmStyle_corr"]), Convert.ToString(Session["prmShipto_corr"]), Convert.ToString(Session["prmDie_corr"]), Convert.ToString(Session["prmCad_corr"]), Convert.ToString(Session["prmPlate_corr"]),Convert.ToString(Session["prmSingle_corr"]), Convert.ToString(Session["prmSet_corr"]), Convert.ToString(Session["prmTandem_corr"]), Convert.ToDecimal(Session["prmWidthFrom_corr"]), Convert.ToDecimal(Session["prmWidthTo_corr"]), Convert.ToDecimal(Session["prmLenFrom_corr"]), Convert.ToDecimal(Session["prmLenTo_corr"]), Convert.ToDecimal(Session["prmDepFrom_corr"]), Convert.ToDecimal(Session["prmDepTo_corr"]), Convert.ToString(Session["prmPartDscr_corr"]));
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            //string cmd = "select * from column_maintain where user_name = '" + UserLogin.UserName + "' and main_program = '" + "corrugated_brow.aspx" + "' and sub_program = '" + "corrugated_brow.aspx" + "' order by seq ";
            string cmd = "select * from column_seq_user where user_name = '" + UserLogin.UserName + "' and main_program = '" + "corrugated_brow.aspx" + "' and sub_program = '" + "corrugated_brow.aspx" + "' and display = 0 order by col_seq ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count > 0)
            {
                Session["initial_val"] = ds.Tables[0].Rows.Count;
                
                for (int j = 0; j < ds.Tables[0].Rows.Count; j++)
                {
                    string fld = ds.Tables[0].Rows[j][4].ToString().Trim().ToString();
                    Int32 seq = Convert.ToInt32(ds.Tables[0].Rows[j][6].ToString().Trim().ToString());
                    //Response.Write(fld + "," +seq);
                    if (ds.Tables[0].Rows[0][7].ToString() == "0")
                    {
                        corr_est.Tables[0].Columns[fld].SetOrdinal(seq-1);
                    }
                }
            }
            else
            {
                string cmd_main = "select col_val, seq_no, display from column_maintenance where  main_program = '" + "corrugated_brow.aspx" + "' and sub_program = '" + "corrugated_brow.aspx" + "' and display = 0  order by seq_no ";
                SqlDataAdapter mainda = new SqlDataAdapter(cmd_main, conn);
                DataSet mainds = new DataSet();
                mainda.Fill(mainds);
                Session["initial_val"] = mainds.Tables[0].Rows.Count;

                for (int j = 0; j < mainds.Tables[0].Rows.Count; j++)
                {
                    string fld = mainds.Tables[0].Rows[j][0].ToString().Trim().ToString();
                    Int32 seq = Convert.ToInt32(mainds.Tables[0].Rows[j][1].ToString().Trim().ToString());
                    //Response.Write(fld + "," + seq);
                    if (mainds.Tables[0].Rows[0][2].ToString() == "0")
                    {
                        corr_est.Tables[0].Columns[fld].SetOrdinal(seq-1);
                    }
                }
            }

                for (int i = 0; i < corr_est.Tables[0].Columns.Count; i++)
                {
                    if (corr_est.Tables[0].Columns[i].ColumnName == "vest")
                    {
                        corr_est.Tables[0].Columns[i].ColumnName = "Estimate";
                    }
                    if (corr_est.Tables[0].Columns[i].ColumnName == "vCust")
                    {
                        corr_est.Tables[0].Columns[i].ColumnName = "Customer";
                    }
                    if (corr_est.Tables[0].Columns[i].ColumnName == "vCustpart")
                    {
                        corr_est.Tables[0].Columns[i].ColumnName = "Cust Part";
                    }
                    if (corr_est.Tables[0].Columns[i].ColumnName == "vQty")
                    {
                        corr_est.Tables[0].Columns[i].ColumnName = "Qty";
                    }
                    if (corr_est.Tables[0].Columns[i].ColumnName == "vOrder")
                    {
                        corr_est.Tables[0].Columns[i].ColumnName = "Order";
                    }
                    if (corr_est.Tables[0].Columns[i].ColumnName == "vFgitem")
                    {
                        corr_est.Tables[0].Columns[i].ColumnName = "Fg Item";
                    }
                    if (corr_est.Tables[0].Columns[i].ColumnName == "vStyle")
                    {
                        corr_est.Tables[0].Columns[i].ColumnName = "Style";
                    }
                    if (corr_est.Tables[0].Columns[i].ColumnName == "vItemName")
                    {
                        corr_est.Tables[0].Columns[i].ColumnName = "Item Name";
                    }
                    if (corr_est.Tables[0].Columns[i].ColumnName == "vFlute")
                    {
                        corr_est.Tables[0].Columns[i].ColumnName = "Flute";
                    }
                    if (corr_est.Tables[0].Columns[i].ColumnName == "vTest")
                    {
                        corr_est.Tables[0].Columns[i].ColumnName = "Test";
                    }
                    if (corr_est.Tables[0].Columns[i].ColumnName == "vQtySet")
                    {
                        corr_est.Tables[0].Columns[i].ColumnName = "Qty/Set";
                    }
                    if (corr_est.Tables[0].Columns[i].ColumnName == "vLength")
                    {
                        corr_est.Tables[0].Columns[i].ColumnName = "Length";
                    }
                    if (corr_est.Tables[0].Columns[i].ColumnName == "vWidth")
                    {
                        corr_est.Tables[0].Columns[i].ColumnName = "Width";
                    }
                    if (corr_est.Tables[0].Columns[i].ColumnName == "vDepth")
                    {
                        corr_est.Tables[0].Columns[i].ColumnName = "Depth";
                    }
                    if (corr_est.Tables[0].Columns[i].ColumnName == "vDie")
                    {
                        corr_est.Tables[0].Columns[i].ColumnName = "Die#";
                    }
                    if (corr_est.Tables[0].Columns[i].ColumnName == "vCad")
                    {
                        corr_est.Tables[0].Columns[i].ColumnName = "Cad#";
                    }
                    if (corr_est.Tables[0].Columns[i].ColumnName == "vPlate")
                    {
                        corr_est.Tables[0].Columns[i].ColumnName = "Plate";
                    }
                    if (corr_est.Tables[0].Columns[i].ColumnName == "vEstDate")
                    {
                        corr_est.Tables[0].Columns[i].ColumnName = "Est Date";
                    }
                    if (corr_est.Tables[0].Columns[i].ColumnName == "vModi")
                    {
                        corr_est.Tables[0].Columns[i].ColumnName = "Modified By";
                    }
                    if (corr_est.Tables[0].Columns[i].ColumnName == "vShipTo")
                    {
                        corr_est.Tables[0].Columns[i].ColumnName = "Ship To ID";
                    }
                    if (corr_est.Tables[0].Columns[i].ColumnName == "vCreatedBy")
                    {
                        corr_est.Tables[0].Columns[i].ColumnName = "Created By";
                    }
                    if (corr_est.Tables[0].Columns[i].ColumnName == "vreckey")
                    {
                        corr_est.Tables[0].Columns[i].ColumnName = "RecKey";
                    }
                    if (corr_est.Tables[0].Columns[i].ColumnName == "vBoard")
                    {
                        corr_est.Tables[0].Columns[i].ColumnName = "Board";
                    }
                }
                DataView dv = corr_est.Tables[0].DefaultView;
               
                if (Session["corr_bro_gridview2_sort"] == null)
                {
                    dv.Sort = "Estimate DESC";
                }
                if (Session["corr_bro_gridview2_sort"] != null)
                {
                    dv.Sort = Convert.ToString(Session["corr_bro_gridview2_sort"]);
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
        

        Session["prmEstimate_corr"] = txt_estimate.Text.Trim();
        Session["prmAction_corr"]   = "Search";
        Session["prmCustomer_corr"] = txt_cust.Text.Trim();
        Session["prmCustpart_corr"] = txt_custpart.Text.Trim();
        Session["prmFgItem_corr"]   = txt_fgitem.Text.Trim();
        Session["prmStyle_corr"]    = txt_style.Text.Trim();
        Session["prmDie_corr"]      = txt_die.Text.Trim();
        Session["prmCad_corr"]      = txt_cad.Text.Trim();
        Session["prmPlate_corr"]    = txt_plate.Text.Trim();
        Session["prmWidthFrom_corr"] = txt_w.Text.Trim();
        Session["prmWidthTo_corr"]  = txt_w2.Text.Trim();

        Session["prmLenFrom_corr"]  = txt_l.Text.Trim();
        Session["prmLenTo_corr"]    = txt_l2.Text.Trim();
        Session["prmDepFrom_corr"]  = txt_d.Text.Trim();
        Session["prmDepTo_corr"]    = txt_d2.Text.Trim();
        Session["prmSingle_corr"]   = HiddenField1.Value;
        Session["prmSet_corr"]      = HiddenField2.Value;
        Session["prmTandem_corr"]   = HiddenField3.Value;
        Session["prmShipto_corr"] = txt_shipto.Text.Trim();
        Session["prmPartDscr_corr"] = txt_itemname.Text.Trim();
        Session["index_list_corrugated"] = null;
        Page_Load(sender, e);             
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
        txt_shipto.Text = str.ToString();
        txt_itemname.Text = str.ToString();

        UserClass UserLogin = (UserClass)Session["User"];
        
        Session["prmEstimate_corr"] = txt_estimate.Text.Trim();
        Session["prmAction_corr"] = "Select";
        Session["prmCustomer_corr"] = txt_cust.Text.Trim();
        Session["prmCustpart_corr"] = txt_custpart.Text.Trim();
        Session["prmFgItem_corr"] = txt_fgitem.Text.Trim();
        Session["prmStyle_corr"] = txt_style.Text.Trim();
        Session["prmDie_corr"] = txt_die.Text.Trim();
        Session["prmCad_corr"] = txt_cad.Text.Trim();
        Session["prmPlate_corr"] = txt_plate.Text.Trim();
        Session["prmWidthFrom_corr"] = txt_w.Text.Trim();
        Session["prmWidthTo_corr"] = txt_w2.Text.Trim();

        Session["prmLenFrom_corr"] = txt_l.Text.Trim();
        Session["prmLenTo_corr"] = txt_l2.Text.Trim();
        Session["prmDepFrom_corr"] = txt_d.Text.Trim();
        Session["prmDepTo_corr"] = txt_d2.Text.Trim();
        Session["prmShipto_corr"] = txt_shipto.Text.Trim();
        Session["prmPartDscr_corr"] = txt_itemname.Text.Trim();
        Session["prmSingle_corr"] = HiddenField1.Value;
        Session["prmSet_corr"] = HiddenField2.Value;
        Session["prmTandem_corr"] = HiddenField3.Value;
        Session["index_list_corrugated"] = null;
        Session["corr_bro_gridview2_sort"] = null;
       
        Page_Load(sender, e);
    }



    //protected void GridView1_RowDataBound(object sender, GridViewRowEventArgs e)
    //{
    //    e.Row.Attributes["onclick"] =
    //        ClientScript.GetPostBackClientHyperlink
    //            (this.GridView1, "Select$" + e.Row.RowIndex);
    //}

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
            if (Session["initial_val"] != null)
            {
                for (int show = Convert.ToInt32(Session["initial_val"]) + 6; show < 35; show++)
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
                    string[] val = e.Row.Cells[i + 6].Text.Split(new char[] { ' ' });
                    e.Row.Cells[i + 6].Text = val[0].ToString();
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
            Session["index_list_corrugated"] = GridView2.SelectedIndex;
            Session["order_corrugated_est"] = ((Label)GridView2.SelectedRow.FindControl("label_est_no")).Text;
            Session["order_corrugated_fgitem"] = ((Label)GridView2.SelectedRow.FindControl("Label_fgitem")).Text;
            Session["order_entry_est_no"] = ((Label)GridView2.SelectedRow.FindControl("label_est_no")).Text;
            Session["item"] = ((Label)GridView2.SelectedRow.FindControl("Label_fgitem")).Text;

            Session["order_corrugated_formno"] = ((Label)GridView2.SelectedRow.FindControl("typeLabel")).Text;
            Session["order_corrugated_blankno"] = ((Label)GridView2.SelectedRow.FindControl("blankLabel")).Text;
            Session["order_rec_key"] = ((Label)GridView2.SelectedRow.FindControl("label_rec_key")).Text;
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

        Session["corr_bro_gridview2_sort"] = sortExpression + direction;
       // BuildDataSource();

    }


}
