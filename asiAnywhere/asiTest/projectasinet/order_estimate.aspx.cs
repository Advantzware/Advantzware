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

public partial class Order_estimate : System.Web.UI.Page
{
    string raw_url;
    private string strItem = "";
    private string rbtext = "";
    private const string ASCENDING = " ASC";
    private const string DESCENDING = " DESC";

    int total;
    Int32 seq_no;
    Int32 f_seq_no;
    protected void Page_PreRender(object sender, EventArgs e)
    {

        BuildDataSource();
        try
        {
            if (Session["index_est"] != null)
            {
                Session["order_entry_est_no"] = ((Label)GridView2.SelectedRow.FindControl("est_label")).Text;
                Session["order_rec_key"] = ((Label)GridView2.SelectedRow.FindControl("lbl_rec_key")).Text;
                Session["Item"] = ((Label)GridView2.SelectedRow.FindControl("Label11")).Text;

            }

            if (Session["index_est"] == null)
            {
                GridView2.SelectedIndex = 0;

                string cc = ((Label)GridView2.SelectedRow.FindControl("CustLabel")).Text;

                Session["order_entry_customer_po"] = ((Label)GridView2.SelectedRow.FindControl("cust_po_label")).Text;
                Session["order_entry_est_no"] = ((Label)GridView2.SelectedRow.FindControl("est_label")).Text;
                Session["order_entry_job_no_1"] = ((Label)GridView2.SelectedRow.FindControl("job_label")).Text;
                Session["order_entry_job_no_2"] = ((Label)GridView2.SelectedRow.FindControl("job2_label")).Text;
                Session["order_entry_status_check"] = ((Label)GridView2.SelectedRow.FindControl("status_label")).Text;

                Session["order_entry_req_date"] = ((Label)GridView2.SelectedRow.FindControl("ReqDateLabel")).Text;
                Session["order_entry_req_code"] = ((Label)GridView2.SelectedRow.FindControl("ReqcodeLabel")).Text;
                Session["order_entry_prom_code"] = ((Label)GridView2.SelectedRow.FindControl("PromCodeLabel")).Text;
                Session["order_entry_prom_date"] = ((Label)GridView2.SelectedRow.FindControl("PromDateLabel")).Text;
                Session["order_entry_over"] = ((Label)GridView2.SelectedRow.FindControl("OverLabel")).Text;
                Session["order_entry_under"] = ((Label)GridView2.SelectedRow.FindControl("UnderLabel")).Text;
                Session["order_entry_sman1"] = ((Label)GridView2.SelectedRow.FindControl("sman1Label")).Text;
                Session["order_entry_sman2"] = ((Label)GridView2.SelectedRow.FindControl("sman2Label")).Text;
                Session["order_entry_sman3"] = ((Label)GridView2.SelectedRow.FindControl("sman3Label")).Text;
                Session["order_entry_spct1"] = ((Label)GridView2.SelectedRow.FindControl("spct1Label")).Text;
                Session["order_entry_spct2"] = ((Label)GridView2.SelectedRow.FindControl("spct2Label")).Text;
                Session["order_entry_spct3"] = ((Label)GridView2.SelectedRow.FindControl("spct3Label")).Text;
                Session["order_entry_scomm1"] = ((Label)GridView2.SelectedRow.FindControl("scommLabel")).Text;
                Session["order_entry_scomm2"] = ((Label)GridView2.SelectedRow.FindControl("scomm2Label")).Text;
                Session["order_entry_scomm3"] = ((Label)GridView2.SelectedRow.FindControl("scomm3Label")).Text;
                Session["order_entry_sname1"] = ((Label)GridView2.SelectedRow.FindControl("sname1Label")).Text;
                Session["order_entry_sname2"] = ((Label)GridView2.SelectedRow.FindControl("sname2Label")).Text;
                Session["order_entry_sname3"] = ((Label)GridView2.SelectedRow.FindControl("sname3Label")).Text;
                Session["order_entry_type"] = ((Label)GridView2.SelectedRow.FindControl("typeLabel")).Text;
                Session["order_entry_tax"] = ((Label)GridView2.SelectedRow.FindControl("taxLabel")).Text;

                Session["order_entry_quote"] = ((Label)GridView2.SelectedRow.FindControl("Quote_Label")).Text;

                foreach (GridViewRow gv in GridView2.Rows)
                {
                    Session["order_rec_key"] = ((Label)GridView2.SelectedRow.FindControl("lbl_rec_key")).Text;

                    string selectedText = ((Label)GridView2.SelectedRow.FindControl("value")).Text;

                    Session["order_est"] = ((Label)GridView2.SelectedRow.FindControl("order_label")).Text;
                    Session["customer_fglookup_val"] = ((Label)GridView2.SelectedRow.FindControl("customer_label")).Text;
                    HiddenField1.Value = ((Label)GridView2.SelectedRow.FindControl("customer_label")).Text;
                    Session["order_entry_cust_no"] = ((Label)GridView2.SelectedRow.FindControl("customer_label")).Text;
                    string selectedText2 = ((Label)GridView2.SelectedRow.FindControl("Label11")).Text;
                    Session["order_entry_cust_quote"] = ((Label)GridView2.SelectedRow.FindControl("order_quote_Label")).Text;
                    Session["item_est"] = selectedText2;
                    Session["view_line_est"] = selectedText;
                    Session["item"] = Session["item_est"];
                    Session["line"] = Session["view_line_est"];
                    Session["order"] = Session["order_est"];

                }
                if (Convert.ToInt32(Session["view_line_est"]) == 0)
                {
                    Session["order_est_value_check"] = null;
                }
                else
                {
                    Session["order_est_value_check"] = 1;
                }
            }
             
            if (Session["order_est"] == null)
            {          
                GridView2.SelectedIndex = -1;
            }
        }
        catch
        {
            //return;
        }
    }
    
    protected void Page_Load(object sender, EventArgs e)
    {
        
        Session["select_order_col_index"] = null;
        Session["genestno"] = null;
        Session["genestcust"] = null;
        Session["prmuomfld"] = null;
        Session["Item_Inquiry_value_check"] = null;
        // Session["view_order_entry_pages"] = "5";
        Session["view_order_entry_pages"] = null;
        Session["view_order_entry_pages_with_estimate"] = "5";
        Session["order_inquiry_status_check"] = null;
        Session["spec_list_notes_index"] = null;
        Session["top_spec_list_notes_index"] = null;
        Session["myneworder"] = null;
        Session["show"] = null;
        Session["order_list_notes_index"] = null;
        Session["top_list_attach_rec_key"] = null;
        Session["item_show_hide_button"] = null;

        Image tick_col = (Image)Master.FindControl("Image6");
        tick_col.Visible = false;

                
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (!Page.IsPostBack)
        {           
            Session["customer_fglookup_val"] = null;
           
            if (ddl_po_status.SelectedIndex == 0)
            {
                Session["ddl_po_status_value"] = "OPEN";
            }
            if (ddl_po_status.SelectedIndex == 1)
            {
                Session["ddl_po_status_value"] = "ANY";
            }
            if (ddl_po_status.SelectedIndex == 2)
            {
                Session["ddl_po_status_value"] = "PENDING";
            }
            if (ddl_po_status.SelectedIndex == 3)
            {
                Session["ddl_po_status_value"] = "CLOSED";
            }
            try
            {
                Session["Rowuser"] = UserLogin.UserName;
                TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
                GridView2.PageSize = Convert.ToInt32(ddl_display.Text);
            }
            catch { }

        }
        
        try
        {
            raw_url = Page.Request.RawUrl;
            string[] url = raw_url.Split(new char[] { '/' });          
            Session["main_prgrm_url"] = url[2].ToString();
            Session["sub_prgrm_url"] = url[2].ToString();
            string selectedText2 = ((Label)GridView2.SelectedRow.FindControl("Label11")).Text;
            Session["item"] = selectedText2;
        }
        catch { }
        

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "order_estimate.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;
            func1 f1 = new func1();
            
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);           
            if (aUsers == "external")
            {
                //customerid.Visible = false;
                //txt_customer.Visible = false;
                //CustomerLook.Visible = false;
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");
            }
        }
                
        Label name = (Label)Master.FindControl("lbl_page");
        name.Text = "Brws Order";
        ddl_order.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_customer.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_customerpo.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_custpart.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_fgitem.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_estimate.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_job1.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_job2.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_quote.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        try
        {
            TextBox rows_per_page = (TextBox)FormView1.FindControl("aLineLabel");
            rows_per_page.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        }
        catch { }

        /*ImageButton brwsorder = (ImageButton)Master.FindControl("brwsorder");
        brwsorder.ImageUrl = "~/img/brwsorder1.jpg";      */  

        if (Session["fgitem1"] != null)
        {
            txt_fgitem.Text = Convert.ToString(Session["fgitem1"]);
            Session["prmFgitem"] = txt_fgitem.Text;
            

            Session["prmCust_entry"] = txt_customer.Text.Trim();
            Session["prmAction_entry"] = "Search";
            Session["prmPonum_entry"] = txt_customerpo.Text.Trim();
            Session["prmPartno_entry"] = txt_custpart.Text.Trim();
            Session["prmPostatus_entry"] = ddl_po_status.SelectedValue.Trim();
            Session["prmOrderNum_entry"] = ddl_order.Text.Trim();
            Session["prmFgitem_entry"] = txt_fgitem.Text.Trim();
            Session["prmEst_entry"] = txt_estimate.Text.Trim();
            Session["prmJob_entry"] = txt_job1.Text.Trim();
            Session["prmJob2_entry"] = txt_job2.Text.Trim();
            Session["prmQuote_entry"] = txt_quote.Text.Trim();

            
            GridView2.SelectedIndex = 0;
            Session["fgitem1"] = null;
            Session["prmFgitem"] = null;

            Session["index_est"] = null;
            Session["order_entry"] = null;

        }
        if (Session["order_est"] != null)
        {
            Session["prmOrderNum"] = Session["temp"];
        }
       
       

        int rows = GridView2.Rows.Count;
        if (rows == Convert.ToInt32(Session["rows"]))
        {
            if (Session["index2_est"] != null)
            {
                Session["index_est"] = Session["index2_est"];
                GridView2.SelectedIndex = Convert.ToInt32(Session["index_est"]);               
                Session["index2_est"] = null;
            }
        }
        try
        {           
            GridView2.SelectedIndex = Convert.ToInt32(Session["index_est"]);
            if (Session["index_est"] != null)
            {
                Session["order_entry_est_no"] = ((Label)GridView2.SelectedRow.FindControl("est_label")).Text;
                Session["order_rec_key"] = ((Label)GridView2.SelectedRow.FindControl("lbl_rec_key")).Text;
                Session["Item"] = ((Label)GridView2.SelectedRow.FindControl("Label11")).Text;
            }
        }
        catch { }
       

        if (Session["gridsize"] != null)
        {            
            GridView2.PageSize = Convert.ToInt32(Session["gridsize"]);
        }        

        try
        {
            TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");            
            Session["size"] = Convert.ToInt32(ddl_display.Text);            
            GridView2.PageSize = Convert.ToInt32(Session["size"]);
        }
        catch
        {
            // return;
        }
        
        
    }
    private void BuildDataSource()
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["prmCust_entry"] == null || Convert.ToString(Session["prmCust_entry"]) == "")
            Session["prmCust_entry"] = "";

        if (Session["prmPonum_entry"] == null || Convert.ToString(Session["prmPonum_entry"]) == "")
            Session["prmPonum_entry"] = "";
        if (Session["prmPartno_entry"] == null || Convert.ToString(Session["prmPartno_entry"]) == "")
            Session["prmPartno_entry"] = "";
        if (Session["prmPostatus_entry"] == null || Convert.ToString(Session["prmPostatus_entry"]) == "")
            Session["prmPostatus_entry"] = "open";
        if (Session["prmOrderNum_entry"] == null || Convert.ToString(Session["prmOrderNum_entry"]) == "")
            Session["prmOrderNum_entry"] = 0;
        if (Session["prmFgitem_entry"] == null || Convert.ToString(Session["prmFgitem_entry"]) == "")
            Session["prmFgitem_entry"] = "";
        if (Session["prmEst_entry"] == null || Convert.ToString(Session["prmEst_entry"]) == "")
            Session["prmEst_entry"] = "";
        if (Session["prmJob_entry"] == null || Convert.ToString(Session["prmJob_entry"]) == "")
            Session["prmJob_entry"] = "";
        if (Session["prmJob2_entry"] == null || Convert.ToString(Session["prmJob2_entry"]) == "")
            Session["prmJob2_entry"] = "";
        if (Session["prmQuote_entry"] == null || Convert.ToString(Session["prmQuote_entry"]) == "")
            Session["prmQuote_entry"] = "";

        if (Convert.ToString(Session["prmAction_entry"]) != "Search" || Convert.ToString(Convert.ToString(Session["prmAction_entry"])) == "" || Session["prmAction_entry"] == null)
        {
            Session["prmAction_entry"] = "Select";
        }
        

        Order entry = new Order();
        DataSet ds_entry = new DataSet();
        ds_entry = entry.SelectOrder(UserLogin.UserName, Convert.ToString(Session["prmCust_entry"]), Convert.ToString(Session["prmAction_entry"]), Convert.ToString(Session["prmPonum_entry"]), Convert.ToString(Session["prmPartno_entry"]), Convert.ToString(Session["prmPostatus_entry"]), Convert.ToString(Session["prmOrderNum_entry"]), Convert.ToString(Session["prmFgitem_entry"]), Convert.ToString(Session["prmEst_entry"]), Convert.ToString(Session["prmJob_entry"]), Convert.ToString(Session["prmJob2_entry"]), Convert.ToString(Session["prmQuote_entry"]));

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            string cmd = "select * from column_seq_user where user_name = '" + UserLogin.UserName + "' and main_program = '" + "order_estimate.aspx" + "' and sub_program = '" + "order_estimate.aspx" + "' and display = 0 order by col_seq ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);
                       
            string cmd2 = "select display from column_seq_user where user_name = '" + UserLogin.UserName + "' and main_program = '" + "order_estimate.aspx" + "' and sub_program = '" + "order_estimate.aspx" + "' and display = 0 ";
            SqlDataAdapter da2 = new SqlDataAdapter(cmd2, conn);
            DataSet ds2 = new DataSet();
            da2.Fill(ds2);
            
            if (ds2.Tables[0].Rows.Count > 0)
            {
                Session["initial_val"] = ds2.Tables[0].Rows.Count;

                for (int j = 0; j < ds.Tables[0].Rows.Count; j++)
                {
                    string fld = ds.Tables[0].Rows[j][4].ToString().Trim().ToString();
                    Int32 seq = Convert.ToInt32(ds.Tables[0].Rows[j][6].ToString().Trim().ToString());
                    
                    if (ds2.Tables[0].Rows[0][0].ToString() == "0")
                    {
                        ds_entry.Tables[0].Columns[fld].SetOrdinal(seq);
                    }
                }

                ds_entry.Tables[0].Columns["Line"].SetOrdinal(27);

                for (int i = 0; i < ds_entry.Tables[0].Columns.Count; i++)
                {
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "ord-no")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Order";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "cust-no")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Customer";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "cust-name")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Customer Name";
                    }

                    if (ds_entry.Tables[0].Columns[i].ColumnName == "req-date")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Due Date";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "price")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Sell Price";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "pr-uom")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Uom";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "t-price")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Extended Price";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "i-no")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "FG Item";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "i-name")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "FG Item Name";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "part-no")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Customer Part";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "po-no")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Customer PO#";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "vEst")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Est#";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "q-no")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Quote#";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "VJob")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Job#";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "VJob2")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Job";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "ord-date")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Order Date";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "stat")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Status";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "qty")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Ordered Qty";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "Prod")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Prod Qty";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "vshipqty")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Shipped Qty";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "vinvqty")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Invoice Qty";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "onhandqty")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "On Hand Quantity";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "actrelqty")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Act Rel Qty";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "wipqty")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Wip Qty/Balance Due";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "oupct")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "O/U%";
                    }
                }

                DataView dv = ds_entry.Tables[0].DefaultView;
                if (Session["order_entry_grid2_sort_ex"] == null)
                {
                    dv.Sort = "Order DESC";
                }
                if (Session["order_entry_grid2_sort_ex"] != null)
                {
                    dv.Sort = Convert.ToString(Session["order_entry_grid2_sort_ex"]);
                }
                
                GridView2.DataSource = dv;
                GridView2.DataBind();
            }
            else
            {
                
                string cmd_main = "select col_val, seq_no, display from column_maintenance where  main_program = '" + "order_estimate.aspx" + "' and sub_program = '" + "order_estimate.aspx" + "' and display = 0  order by seq_no ";
                SqlDataAdapter mainda = new SqlDataAdapter(cmd_main, conn);
                DataSet mainds = new DataSet();
                mainda.Fill(mainds);
                Session["initial_val"] = mainds.Tables[0].Rows.Count;
                
                for (int j = 0; j < mainds.Tables[0].Rows.Count; j++)
                {
                    string fld = mainds.Tables[0].Rows[j][0].ToString().Trim().ToString();
                    Int32 seq = Convert.ToInt32(mainds.Tables[0].Rows[j][1].ToString().Trim().ToString());
                   
                    if (mainds.Tables[0].Rows[0][2].ToString() == "0")
                    {
                        ds_entry.Tables[0].Columns[fld].SetOrdinal(seq);
                    }

                }
                ds_entry.Tables[0].Columns["Line"].SetOrdinal(27);
                for (int i = 0; i < ds_entry.Tables[0].Columns.Count; i++)
                {
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "ord-no")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Order";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "cust-no")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Customer";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "cust-name")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Customer Name";
                    }

                    if (ds_entry.Tables[0].Columns[i].ColumnName == "req-date")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Due Date";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "price")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Sell Price";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "pr-uom")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Uom";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "t-price")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Extended Price";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "i-no")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "FG Item";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "i-name")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "FG Item Name";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "part-no")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Customer Part";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "po-no")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Customer PO#";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "vEst")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Est#";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "q-no")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Quote#";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "VJob")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Job#";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "VJob2")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Job";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "ord-date")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Order Date";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "stat")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Status";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "qty")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Ordered Qty";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "Prod")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Prod Qty";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "vshipqty")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Shipped Qty";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "vinvqty")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Invoice Qty";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "onhandqty")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "On Hand Quantity";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "actrelqty")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Act Rel Qty";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "wipqty")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "Wip Qty/Balance Due";
                    }
                    if (ds_entry.Tables[0].Columns[i].ColumnName == "oupct")
                    {
                        ds_entry.Tables[0].Columns[i].ColumnName = "O/U%";
                    }
                }
                DataView dv = ds_entry.Tables[0].DefaultView;
                if (Session["order_entry_grid2_sort_ex"] == null)
                {
                    dv.Sort = "Order DESC";
                }
                if (Session["order_entry_grid2_sort_ex"] != null)
                {
                    dv.Sort = Convert.ToString(Session["order_entry_grid2_sort_ex"]);
                }
                GridView2.DataSource = dv;
                GridView2.DataBind();
            }
        }
        catch { }        
    }
    protected void btnSearch_Click(object sender, EventArgs e)
    {
        UserClass UserLogin = (UserClass)Session["User"];
        
        Session["prmCust_entry"] = txt_customer.Text.Trim();
        Session["prmAction_entry"] = "Search";
        Session["prmPonum_entry"] = txt_customerpo.Text.Trim();
        Session["prmPartno_entry"] = txt_custpart.Text.Trim();
        Session["prmPostatus_entry"] = ddl_po_status.SelectedValue.Trim();
        Session["prmOrderNum_entry"] = ddl_order.Text.Trim();
        Session["prmFgitem_entry"] = txt_fgitem.Text.Trim();
        Session["prmEst_entry"] = txt_estimate.Text.Trim();
        Session["prmJob_entry"] = txt_job1.Text.Trim();
        Session["prmJob2_entry"] = txt_job2.Text.Trim();
        Session["prmQuote_entry"] = txt_quote.Text.Trim();
        Session["temp"] = ddl_order.Text;
        Session["index_est"] = null;
        Session["order_est"] = null;
        Session["order"] = null;
        Session["line"] = null;
        Session["item_est"] = null;
        Session["view_line_est"] = null;
        Session["index_est"] = null;
        Session["index_est2"] = null;
        Session["order_entry_status_check"] = null;
                
        Page_Load(sender, e);
    }

    protected void btn_reset_Click(object sender, EventArgs e)
    {
        string str = "";
        TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
        ddl_order.Text = str.ToString();
        txt_customer.Text = str.ToString();
        txt_customerpo.Text = str.ToString();
        txt_custpart.Text = str.ToString();
        txt_estimate.Text = str.ToString();
        txt_fgitem.Text = str.ToString();
        txt_job1.Text = str.ToString();
        txt_job2.Text = str.ToString();
        txt_quote.Text = str.ToString();
        ddl_po_status.SelectedIndex = 0;

        UserClass UserLogin = (UserClass)Session["User"];
        
        Session["prmCust_entry"] = txt_customer.Text.Trim();
        Session["prmAction_entry"] = "Select";
        Session["prmPonum_entry"] = txt_customerpo.Text.Trim();
        Session["prmPartno_entry"] = txt_custpart.Text.Trim();
        Session["prmPostatus_entry"] = ddl_po_status.SelectedValue.Trim();
        Session["prmOrderNum_entry"] = ddl_order.Text.Trim();
        Session["prmFgitem_entry"] = txt_fgitem.Text.Trim();
        Session["prmEst_entry"] = txt_estimate.Text.Trim();
        Session["prmJob_entry"] = txt_job1.Text.Trim();
        Session["prmJob2_entry"] = txt_job2.Text.Trim();
        Session["prmQuote_entry"] = txt_quote.Text.Trim();
        Session["temp"] = null;
        Session["index_est"] = null;
        Session["order_est"] = null;
        Session["item"] = null;
        Session["line"] = null;
        Session["index_est"] = null;
        Session["index2_est"] = null;
        Session["gridsize"] = null;
        Session["view_line"] = null;
        Session["order"] = null;

        Session["order_entry_req_date"] = null;
        Session["order_entry_req_code"] = null;
        Session["order_entry_prom_code"] = null;
        Session["order_entry_prom_date"] = null;
        Session["order_entry_over"] = null;
        Session["order_entry_under"] = null;
        Session["order_entry_sman1"] = null;
        Session["order_entry_sman2"] = null;
        Session["order_entry_sman3"] = null;
        Session["order_entry_spct1"] = null;
        Session["order_entry_spct2"] = null;
        Session["order_entry_spct3"] = null;
        Session["order_entry_scomm1"] = null;
        Session["order_entry_scomm2"] = null;
        Session["order_entry_scomm3"] = null;
        Session["order_entry_sname1"] = null;
        Session["order_entry_sname2"] = null;
        Session["order_entry_sname3"] = null;
        Session["order_entry_type"] = null;
        Session["order_entry_tax"] = null;
        Session["order_entry_customer_po"] = null;
        Session["order_entry_est_no"] = null;
        Session["order_entry_quote"] = null;
        Session["order_entry_job_no_1"] = null;
        Session["order_entry_job_no_2"] = null;
        Session["order_entry_status_check"] = null;
        Session["order_entry_cust_quote"] = null;
        Session["order_rec_key"] = null;
        Session["order_spec_note_est"] = null;
        
        Session["order_entry_grid2_sort_ex"] = null;
        
        Page_Load(sender, e);
    }

       
    protected void GridView2_PageIndexChanging(object sender, GridViewPageEventArgs e)
    {
        GridView2.PageIndex = e.NewPageIndex;         
               
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

        Session["order_entry_grid2_sort_ex"] = sortExpression + direction;
                      
    }
    
    protected void ddl_display_TextChanged(object sender, EventArgs e)
    {
        TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
        Session["gridsize"] = ddl_display.Text;        
        ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);

    }
    
    protected void ddl_po_status_SelectedIndexChanged(object sender, EventArgs e)
    {
        if (ddl_po_status.SelectedIndex == 0)
        {
            Session["ddl_po_status_value"] = "OPEN";
        }
        if (ddl_po_status.SelectedIndex == 1)
        {
            Session["ddl_po_status_value"] = "ANY";
        }
        if (ddl_po_status.SelectedIndex == 2)
        {
            Session["ddl_po_status_value"] = "PENDING";
        }
        if (ddl_po_status.SelectedIndex == 3)
        {
            Session["ddl_po_status_value"] = "CLOSED";
        }
        btnSearch_Click(sender, e);
    }
    protected void GridView2_SelectedIndexChanged(object sender, EventArgs e)
    {
        
        int index_est = GridView2.SelectedIndex;
        Session["index_est"] = index_est;

        try
        {           
            Session["order_rec_key"] = ((Label)GridView2.SelectedRow.FindControl("lbl_rec_key")).Text;
            Session["order_entry_customer_po"] = ((Label)GridView2.SelectedRow.FindControl("cust_po_label")).Text;
            Session["order_entry_est_no"] = ((Label)GridView2.SelectedRow.FindControl("est_label")).Text;
            Session["order_entry_job_no_1"] = ((Label)GridView2.SelectedRow.FindControl("job_label")).Text;
            Session["order_entry_job_no_2"] = ((Label)GridView2.SelectedRow.FindControl("job2_label")).Text;
            Session["order_entry_status_check"] = ((Label)GridView2.SelectedRow.FindControl("status_label")).Text;
            Session["order_entry_req_date"] = ((Label)GridView2.SelectedRow.FindControl("ReqDateLabel")).Text;
            Session["order_entry_req_code"] = ((Label)GridView2.SelectedRow.FindControl("ReqcodeLabel")).Text;
            Session["order_entry_prom_code"] = ((Label)GridView2.SelectedRow.FindControl("PromCodeLabel")).Text;
            Session["order_entry_prom_date"] = ((Label)GridView2.SelectedRow.FindControl("PromDateLabel")).Text;
            Session["order_entry_over"] = ((Label)GridView2.SelectedRow.FindControl("OverLabel")).Text;
            Session["order_entry_under"] = ((Label)GridView2.SelectedRow.FindControl("UnderLabel")).Text;
            Session["order_entry_sman1"] = ((Label)GridView2.SelectedRow.FindControl("sman1Label")).Text;
            Session["order_entry_sman2"] = ((Label)GridView2.SelectedRow.FindControl("sman2Label")).Text;
            Session["order_entry_sman3"] = ((Label)GridView2.SelectedRow.FindControl("sman3Label")).Text;
            Session["order_entry_spct1"] = ((Label)GridView2.SelectedRow.FindControl("spct1Label")).Text;
            Session["order_entry_spct2"] = ((Label)GridView2.SelectedRow.FindControl("spct2Label")).Text;
            Session["order_entry_spct3"] = ((Label)GridView2.SelectedRow.FindControl("spct3Label")).Text;
            Session["order_entry_scomm1"] = ((Label)GridView2.SelectedRow.FindControl("scommLabel")).Text;
            Session["order_entry_scomm2"] = ((Label)GridView2.SelectedRow.FindControl("scomm2Label")).Text;
            Session["order_entry_scomm3"] = ((Label)GridView2.SelectedRow.FindControl("scomm3Label")).Text;
            Session["order_entry_sname1"] = ((Label)GridView2.SelectedRow.FindControl("sname1Label")).Text;
            Session["order_entry_sname2"] = ((Label)GridView2.SelectedRow.FindControl("sname2Label")).Text;
            Session["order_entry_sname3"] = ((Label)GridView2.SelectedRow.FindControl("sname3Label")).Text;
            Session["order_entry_type"] = ((Label)GridView2.SelectedRow.FindControl("typeLabel")).Text;
            Session["order_entry_tax"] = ((Label)GridView2.SelectedRow.FindControl("taxLabel")).Text;
            Session["order_entry_quote"] = ((Label)GridView2.SelectedRow.FindControl("Quote_Label")).Text;

            string cc = ((Label)GridView2.SelectedRow.FindControl("CustLabel")).Text;

            foreach (GridViewRow gv in GridView2.Rows)
            {
                string selectedText = ((Label)GridView2.SelectedRow.FindControl("value")).Text;
                Session["order_est"] = ((Label)GridView2.SelectedRow.FindControl("order_label")).Text;
                Session["customer_fglookup_val"] = ((Label)GridView2.SelectedRow.FindControl("customer_label")).Text;
                HiddenField1.Value = ((Label)GridView2.SelectedRow.FindControl("customer_label")).Text;
                Session["order_entry_cust_no"] = ((Label)GridView2.SelectedRow.FindControl("customer_label")).Text;
                string selectedText2 = ((Label)GridView2.SelectedRow.FindControl("Label11")).Text;
                Session["order_entry_cust_quote"] = ((Label)GridView2.SelectedRow.FindControl("order_quote_Label")).Text;
                Session["item_est"] = selectedText2;
                Session["view_line_est"] = selectedText;
                Session["item"] = Session["item_est"];
                Session["line"] = Session["view_line_est"];
                Session["order"] = Session["order_est"];
            }

            if (Convert.ToInt32(Session["view_line_est"]) == 0)
            {
                Session["order_est_value_check"] = null;
            }
            else
            {
                Session["order_est_value_check"] = 1;
            }
        }
        catch
        {
        }       
    }
    protected void GridView2_RowCreated(object sender, GridViewRowEventArgs e)
    {
        try
        {
            if (Session["initial_val"] != null)
            {
                
                for (int show = Convert.ToInt32(Session["initial_val"]) + 35; show < 60; show++)
                {
                    e.Row.Cells[show].Visible = false;
                }
                e.Row.Cells[60].Visible = false;
                e.Row.Cells[61].Visible = false;
                e.Row.Cells[62].Visible = false;
                e.Row.Cells[63].Visible = false;
                e.Row.Cells[64].Visible = false;
                e.Row.Cells[65].Visible = false;
                e.Row.Cells[66].Visible = false;
                e.Row.Cells[67].Visible = false;
                e.Row.Cells[68].Visible = false;
                e.Row.Cells[69].Visible = false;
                e.Row.Cells[70].Visible = false;
                e.Row.Cells[71].Visible = false;
                e.Row.Cells[72].Visible = false;
                e.Row.Cells[73].Visible = false;
                e.Row.Cells[74].Visible = false;
                e.Row.Cells[75].Visible = false;
                e.Row.Cells[76].Visible = false;
                e.Row.Cells[77].Visible = false;
                e.Row.Cells[78].Visible = false;
                e.Row.Cells[79].Visible = false;
                e.Row.Cells[80].Visible = false;
                e.Row.Cells[81].Visible = false;
                e.Row.Cells[82].Visible = false;
               

            }

        }
        catch { }
    }
    protected void GridView2_RowDataBound(object sender, GridViewRowEventArgs e)
    {        
        if (e.Row.RowType == DataControlRowType.DataRow)
        {
            DataRowView drv = (DataRowView)e.Row.DataItem;
            for (int i = 0; i < drv.DataView.Table.Columns.Count; i++)
            {
                if (drv.DataView.Table.Columns[i].ColumnName.Equals("Due Date"))
                {
                    string[] val = e.Row.Cells[i + 35].Text.Split(new char[] { ' ' });
                    e.Row.Cells[i + 35].Text = val[0].ToString();
                }
                if (drv.DataView.Table.Columns[i].ColumnName.Equals("Order Date"))
                {
                    string[] val = e.Row.Cells[i + 35].Text.Split(new char[] { ' ' });
                    e.Row.Cells[i + 35].Text = val[0].ToString();
                }
            }
        }  
        for (int i = 0; i < e.Row.Cells.Count; i++)
        {
            e.Row.Cells[i].Attributes.Add("style", "white-space: nowrap;");
        }

    }
  
    
}
