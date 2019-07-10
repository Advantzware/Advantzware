
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
#endregion

public partial class topbtnorderreport : System.Web.UI.Page
{



    protected void Page_Load(object sender, System.EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "topbtnorderreport.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            Session["ord_rep_login"] = PrmComp;
            if (aUsers == "external")
            {
                if (!Page.IsPostBack)
                {

                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'topbtnorderreport.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {
                            begincust_TextBox.Text = dr["field1"].ToString();
                            endcust_TextBox.Text = dr["field1"].ToString();
                            
                           
                            if (dr["rd_field1"].ToString() == "1")
                                rdl_openclose.SelectedIndex = 0;
                            if (dr["rd_field1"].ToString() == "2")
                                rdl_openclose.SelectedIndex = 1;                          

                            
                            if (dr["chk_field1"].ToString() == "Yes")
                                chk_order.Checked = true;
                            else
                                chk_order.Checked = false;

                            if (dr["chk_field2"].ToString() == "Yes")
                                chk_cust.Checked = true;
                            else
                                chk_cust.Checked = false;
                            if (dr["chk_field3"].ToString() == "Yes")
                                chk_ord_date.Checked = true;
                            else
                                chk_ord_date.Checked = false;

                            if (dr["chk_field4"].ToString() == "Yes")
                                chk_fg_item.Checked = true;
                            else
                                chk_fg_item.Checked = false;
                            if (dr["chk_field5"].ToString() == "Yes")
                                chk_cust_part.Checked = true;
                            else
                                chk_cust_part.Checked = false;

                            if (dr["chk_field6"].ToString() == "Yes")
                                chk_item_name.Checked = true;
                            else
                                chk_item_name.Checked = false;
                            if (dr["chk_field7"].ToString() == "Yes")
                                chk_cust_po.Checked = true;
                            else
                                chk_cust_po.Checked = false;

                            if (dr["chk_field8"].ToString() == "Yes")
                                chk_ord_qty.Checked = true;
                            else
                                chk_ord_qty.Checked = false;
                            if (dr["chk_field9"].ToString() == "Yes")
                                chk_prod_qty.Checked = true;
                            else
                                chk_prod_qty.Checked = false;                           
                            if (dr["chk_field10"].ToString() == "Yes")
                                chk_ship_qty.Checked = true;
                            else
                                chk_ship_qty.Checked = false;

                            if (dr["chk_fiel11"].ToString() == "Yes")
                                chk_on_hand_qty.Checked = true;
                            else
                                chk_on_hand_qty.Checked = false;
                            if (dr["chk_field12"].ToString() == "Yes")
                                chk_sell_price.Checked = true;
                            else
                                chk_sell_price.Checked = false;

                            if (dr["chk_field13"].ToString() == "Yes")
                                chk_uom.Checked = true;
                            else
                                chk_uom.Checked = false;
                            if (dr["chk_field14"].ToString() == "Yes")
                                chk_unit_count.Checked = true;
                            else
                                chk_unit_count.Checked = false;

                            if (dr["chk_field15"].ToString() == "Yes")
                                chk_pallet_count.Checked = true;
                            else
                                chk_pallet_count.Checked = false;
                            if (dr["chk_field16"].ToString() == "Yes")
                                chk_skids.Checked = true;
                            else
                                chk_skids.Checked = false;

                            if (dr["chk_field17"].ToString() == "Yes")
                                chk_status.Checked = true;
                            else
                                chk_status.Checked = false;
                            if (dr["chk_field18"].ToString() == "Yes")
                                chk_due_date.Checked = true;
                            else
                                chk_due_date.Checked = false;                           

                            if (dr["chk_field19"].ToString() == "Yes")
                                chk_cust_name.Checked = true;
                            else
                                chk_cust_name.Checked = false;

                            if (dr["chk_field20"].ToString() == "Yes")
                                chk_est.Checked = true;
                            else
                                chk_est.Checked = false;
                            if (dr["chk_field21"].ToString() == "Yes")
                                chk_job.Checked = true;
                            else
                                chk_job.Checked = false;

                            if (dr["chk_field22"].ToString() == "Yes")
                                chk_cad.Checked = true;
                            else
                                chk_cad.Checked = false;
                            if (dr["chk_field23"].ToString() == "Yes")
                                chk_inv_qty.Checked = true;
                            else
                                chk_inv_qty.Checked = false;

                            if (dr["chk_field24"].ToString() == "Yes")
                                chk_act_rel_qty.Checked = true;
                            else
                                chk_act_rel_qty.Checked = false;
                            if (dr["chk_field25"].ToString() == "Yes")
                                chk_prod_bal.Checked = true;
                            else
                                chk_prod_bal.Checked = false;

                            if (dr["chk_field26"].ToString() == "Yes")
                                chk_ou.Checked = true;
                            else
                                chk_ou.Checked = false;
                            

                        }
                    }
                    catch
                    {
                        conn.Close();
                    }
                    finally
                    {
                        conn.Close();
                    }
                    endcust_TextBox.ReadOnly=true;
                    OutputLabel.Visible = false;
                HyperLink1.Visible = false;                                 
                begincust_TextBox.Focus();

                }
            }

            if (aUsers == "internal")
            {
                if (!Page.IsPostBack)
                {

                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'topbtnorderreport.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {
                            begincust_TextBox.Text = dr["field1"].ToString();
                            endcust_TextBox.Text = dr["field2"].ToString();
                            
                           
                            if (dr["rd_field1"].ToString() == "1")
                                rdl_openclose.SelectedIndex = 0;
                            if (dr["rd_field1"].ToString() == "2")
                                rdl_openclose.SelectedIndex = 1;                          

                            
                            if (dr["chk_field1"].ToString() == "Yes")
                                chk_order.Checked = true;
                            else
                                chk_order.Checked = false;

                            if (dr["chk_field2"].ToString() == "Yes")
                                chk_cust.Checked = true;
                            else
                                chk_cust.Checked = false;
                            if (dr["chk_field3"].ToString() == "Yes")
                                chk_ord_date.Checked = true;
                            else
                                chk_ord_date.Checked = false;

                            if (dr["chk_field4"].ToString() == "Yes")
                                chk_fg_item.Checked = true;
                            else
                                chk_fg_item.Checked = false;
                            if (dr["chk_field5"].ToString() == "Yes")
                                chk_cust_part.Checked = true;
                            else
                                chk_cust_part.Checked = false;

                            if (dr["chk_field6"].ToString() == "Yes")
                                chk_item_name.Checked = true;
                            else
                                chk_item_name.Checked = false;
                            if (dr["chk_field7"].ToString() == "Yes")
                                chk_cust_po.Checked = true;
                            else
                                chk_cust_po.Checked = false;

                            if (dr["chk_field8"].ToString() == "Yes")
                                chk_ord_qty.Checked = true;
                            else
                                chk_ord_qty.Checked = false;
                            if (dr["chk_field9"].ToString() == "Yes")
                                chk_prod_qty.Checked = true;
                            else
                                chk_prod_qty.Checked = false;                           
                            if (dr["chk_field10"].ToString() == "Yes")
                                chk_ship_qty.Checked = true;
                            else
                                chk_ship_qty.Checked = false;

                            if (dr["chk_field11"].ToString() == "Yes")
                                chk_on_hand_qty.Checked = true;
                            else
                                chk_on_hand_qty.Checked = false;
                            if (dr["chk_field12"].ToString() == "Yes")
                                chk_sell_price.Checked = true;
                            else
                                chk_sell_price.Checked = false;

                            if (dr["chk_field13"].ToString() == "Yes")
                                chk_uom.Checked = true;
                            else
                                chk_uom.Checked = false;
                            if (dr["chk_field14"].ToString() == "Yes")
                                chk_unit_count.Checked = true;
                            else
                                chk_unit_count.Checked = false;

                            if (dr["chk_field15"].ToString() == "Yes")
                                chk_pallet_count.Checked = true;
                            else
                                chk_pallet_count.Checked = false;
                            if (dr["chk_field16"].ToString() == "Yes")
                                chk_skids.Checked = true;
                            else
                                chk_skids.Checked = false;

                            if (dr["chk_field17"].ToString() == "Yes")
                                chk_status.Checked = true;
                            else
                                chk_status.Checked = false;
                            if (dr["chk_field18"].ToString() == "Yes")
                                chk_due_date.Checked = true;
                            else
                                chk_due_date.Checked = false;                           

                            if (dr["chk_field19"].ToString() == "Yes")
                                chk_cust_name.Checked = true;
                            else
                                chk_cust_name.Checked = false;

                            if (dr["chk_field20"].ToString() == "Yes")
                                chk_est.Checked = true;
                            else
                                chk_est.Checked = false;
                            if (dr["chk_field21"].ToString() == "Yes")
                                chk_job.Checked = true;
                            else
                                chk_job.Checked = false;

                            if (dr["chk_field22"].ToString() == "Yes")
                                chk_cad.Checked = true;
                            else
                                chk_cad.Checked = false;
                            if (dr["chk_field23"].ToString() == "Yes")
                                chk_inv_qty.Checked = true;
                            else
                                chk_inv_qty.Checked = false;

                            if (dr["chk_field24"].ToString() == "Yes")
                                chk_act_rel_qty.Checked = true;
                            else
                                chk_act_rel_qty.Checked = false;
                            if (dr["chk_field25"].ToString() == "Yes")
                                chk_prod_bal.Checked = true;
                            else
                                chk_prod_bal.Checked = false;

                            if (dr["chk_field26"].ToString() == "Yes")
                                chk_ou.Checked = true;
                            else
                                chk_ou.Checked = false;
                            

                        }
                    }
                    catch
                    {
                        conn.Close();
                    }
                    finally
                    {
                        conn.Close();
                    }

                    OutputLabel.Visible = false;
                HyperLink1.Visible = false;                
                
                begincust_TextBox.Focus();

                }
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }


        

    }
    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }
    protected void SubmitButton_Click(object sender, EventArgs e)
    {


        //UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (RadioButtonList8.SelectedIndex == 0)
            hid_prmout.Value = "No";
        if (rdl_openclose.SelectedIndex == 0)
            hid_open_close.Value = "1";
        if (rdl_openclose.SelectedIndex == 1)
            hid_open_close.Value = "2";

        if (chk_act_rel_qty.Checked)
            hid_act_rel_qty.Value = "Yes";
        if(chk_cad.Checked)
            hid_cad.Value="Yes";
        if (chk_cust_po.Checked)
            hid_cust_po.Value = "Yes";
        if (chk_cust.Checked)
            hid_cust.Value = "Yes";
        if (chk_cust_name.Checked)
            hid_cust_name.Value = "Yes";
        if (chk_cust_part.Checked)
            hid_cust_part.Value = "Yes";
        if (chk_due_date.Checked)
            hid_due_date.Value = "Yes";
        if (chk_est.Checked)
            hid_est.Value = "Yes";
        if (chk_fg_item.Checked)
            hid_fgitem.Value = "Yes";
        if (chk_inv_qty.Checked)
            hid_inv_qty.Value = "Yes";
        if (chk_item_name.Checked)
            hid_item_name.Value = "Yes";
        if (chk_job.Checked)
            hid_job.Value = "Yes";
        if (chk_on_hand_qty.Checked)
            hid_on_hand_qty.Value = "Yes";
        if (chk_ord_date.Checked)
            hid_ord_date.Value = "Yes";
        if (chk_ord_qty.Checked)
            hid_ord_qty.Value = "Yes";
        if (chk_order.Checked)
            hid_order.Value = "Yes";
        if (chk_ou.Checked)
            hid_ou.Value = "Yes";
        if (chk_pallet_count.Checked)
            hid_pallet_count.Value = "Yes";
        if (chk_prod_bal.Checked)
            hid_prod_bal.Value = "Yes";
        if (chk_prod_qty.Checked)
            hid_prod_qty.Value = "Yes";
        if (chk_sell_price.Checked)
            hid_sell_price.Value = "Yes";
        if (chk_ship_qty.Checked)
            hid_ship_qty.Value = "Yes";
        if (chk_skids.Checked)
            hid_skids.Value = "Yes";
        if (chk_status.Checked)
            hid_status.Value = "Yes";
        if (chk_unit_count.Checked)
            hid_unit_count.Value = "Yes";
        if (chk_uom.Checked)
            hid_uom.Value = "Yes";


        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Order";
        //ObjectDataSource1.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Trans_login"]);
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = "Yes";
        ObjectDataSource1.SelectParameters["prmopenclose"].DefaultValue = hid_open_close.Value;
        ObjectDataSource1.SelectParameters["prmBeginCust"].DefaultValue = begincust_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = endcust_TextBox.Text.Trim();

        ObjectDataSource1.SelectParameters["prmOrder"].DefaultValue = hid_order.Value;
        ObjectDataSource1.SelectParameters["prmCust"].DefaultValue = hid_cust.Value;
        ObjectDataSource1.SelectParameters["prmOrderDate"].DefaultValue = hid_ord_date.Value;
        ObjectDataSource1.SelectParameters["prmFgItem"].DefaultValue = hid_fgitem.Value;
        ObjectDataSource1.SelectParameters["prmCustPart"].DefaultValue = hid_cust_part.Value;
        ObjectDataSource1.SelectParameters["prmItemName"].DefaultValue = hid_item_name.Value;
        ObjectDataSource1.SelectParameters["prmCustPo"].DefaultValue = hid_cust_po.Value;
        ObjectDataSource1.SelectParameters["prmOrderQty"].DefaultValue = hid_ord_qty.Value;

        ObjectDataSource1.SelectParameters["prmProdQty"].DefaultValue = hid_prod_qty.Value;
        ObjectDataSource1.SelectParameters["prmShipQty"].DefaultValue = hid_ship_qty.Value;
        ObjectDataSource1.SelectParameters["prmOnHandQty"].DefaultValue = hid_on_hand_qty.Value;
        ObjectDataSource1.SelectParameters["prmSellPrice"].DefaultValue = hid_sell_price.Value;
        ObjectDataSource1.SelectParameters["prmUom"].DefaultValue = hid_uom.Value;
        ObjectDataSource1.SelectParameters["prmUnitCost"].DefaultValue = hid_unit_count.Value;
        ObjectDataSource1.SelectParameters["prmPalletCount"].DefaultValue = hid_pallet_count.Value;
        ObjectDataSource1.SelectParameters["prmSkids"].DefaultValue = hid_skids.Value;
        ObjectDataSource1.SelectParameters["prmStatus"].DefaultValue = hid_status.Value;
        ObjectDataSource1.SelectParameters["prmDueDate"].DefaultValue = hid_due_date.Value;
        ObjectDataSource1.SelectParameters["prmCustName"].DefaultValue = hid_cust_name.Value;
        ObjectDataSource1.SelectParameters["prmEst"].DefaultValue = hid_est.Value;
        ObjectDataSource1.SelectParameters["prmJob"].DefaultValue = hid_job.Value;
        ObjectDataSource1.SelectParameters["prmCad"].DefaultValue = hid_cad.Value;
        ObjectDataSource1.SelectParameters["prmInvoiceQty"].DefaultValue = hid_inv_qty.Value;
        ObjectDataSource1.SelectParameters["prmActRelQty"].DefaultValue = hid_act_rel_qty.Value;
        ObjectDataSource1.SelectParameters["prmProdBal"].DefaultValue = hid_prod_bal.Value;
        ObjectDataSource1.SelectParameters["prmOU"].DefaultValue = hid_ou.Value;

        
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'topbtnorderreport.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name, field1, field2, rd_field1, chk_field1, chk_field2,chk_field3,chk_field4,chk_field5,chk_field6,chk_field7,chk_field8,chk_field9, chk_field10, chk_field11,chk_field12,chk_field13,chk_field14,chk_field15,chk_field16,chk_field17,chk_field18 , chk_field19, chk_field20,chk_field21,chk_field22,chk_field23,chk_field24,chk_field25,chk_field26) values ('" + UserLogin.UserName + "','topbtnorderreport.aspx','" + begincust_TextBox.Text.Trim() + "','" + endcust_TextBox.Text.Trim() + "','" + hid_open_close.Value + "','" + hid_order.Value + "','" + hid_cust.Value + "','" + hid_ord_date.Value + "','" + hid_fgitem.Value + "','" + hid_cust_part.Value + "','" + hid_item_name.Value + "','" + hid_cust_po.Value + "','" + hid_ord_qty.Value + "','" + hid_prod_qty.Value + "','" + hid_ship_qty.Value + "','" + hid_on_hand_qty.Value + "','" + hid_sell_price.Value + "','" + hid_uom.Value + "','" + hid_unit_count.Value + "','" + hid_pallet_count.Value + "','" + hid_skids.Value + "','" + hid_status.Value + "','" + hid_due_date.Value + "','" + hid_cust_name.Value + "','" + hid_est.Value + "','" + hid_job.Value + "','" + hid_cad.Value + "','" + hid_inv_qty.Value + "','" + hid_act_rel_qty.Value + "','" + hid_prod_bal.Value + "','" + hid_ou.Value + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + begincust_TextBox.Text.Trim() + "', field2 = '" + endcust_TextBox.Text.Trim() + "', rd_field1 = '" + hid_open_close.Value + "', chk_field1 ='" + hid_order.Value + "', chk_field2 ='" + hid_cust.Value + "',chk_field3 ='" + hid_ord_date.Value + "',chk_field4 ='" + hid_fgitem.Value + "',chk_field5 ='" + hid_cust_part.Value + "',chk_field6 ='" + hid_item_name.Value + "',chk_field7 ='" + hid_cust_po.Value + "',chk_field8 ='" + hid_ord_qty.Value + "',chk_field9 ='" + hid_prod_qty.Value + "',chk_field10 ='" + hid_ship_qty.Value + "',chk_field11 ='" + hid_on_hand_qty.Value + "',chk_field12 ='" + hid_sell_price.Value + "',chk_field13 ='" + hid_uom.Value + "',chk_field14 ='" + hid_unit_count.Value + "',chk_field15 ='" + hid_pallet_count.Value + "',chk_field16 ='" + hid_skids.Value + "',chk_field17 ='" + hid_status.Value + "',chk_field18 ='" + hid_due_date.Value + "',chk_field19 ='" + hid_cust_name.Value + "',chk_field20 ='" + hid_est.Value + "',chk_field21 ='" + hid_job.Value + "',chk_field22 ='" + hid_cad.Value + "',chk_field23 ='" + hid_inv_qty.Value + "',chk_field24 ='" + hid_act_rel_qty.Value + "',chk_field25 ='" + hid_prod_bal.Value + "',chk_field26 ='" + hid_ou.Value + "' where user_name = '" + UserLogin.UserName + "' and prog_name ='topbtnorderreport.aspx' ", conn);
                cmd_update.ExecuteNonQuery();
            }
        }
        catch (Exception ex)
        {
            Label1.Text = "Error :" + ex.Message + "<p>";
            conn.Close();
        }
        finally
        {
            conn.Close();
        }





        try
        {
            OutputLabel.Visible = true;
            HyperLink1.Visible = true;
            Label path = (Label)FormView1.FindControl("vFileLabel");
            HyperLink1.Text = path.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + path.Text;
            

            if (path.Text == "")
            {
                Label1.Text = "No Csv Exists";
                Response.Write("<script>window.location.href='topbtnorderreport.aspx'</script>");
            }
        }
        catch
        {

        }
        ////if (Label1.Text == "")
        ////{
        ////    Response.Write("<script>window.location.href='inv_by_salesman.aspx'</script>");
        ////}
    }
    
}
